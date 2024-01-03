{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

-----------------------------------------------------------------------------
--
-- Code generation for ticky-ticky profiling
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

{- OVERVIEW: ticky ticky profiling

Please see
https://gitlab.haskell.org/ghc/ghc/wikis/debugging/ticky-ticky and also
edit it and the rest of this comment to keep them up-to-date if you
change ticky-ticky. Thanks!

 *** All allocation ticky numbers are in bytes. ***

Some of the relevant source files:

       ***not necessarily an exhaustive list***

  * some codeGen/ modules import this one

  * this module imports GHC.Cmm.CLabel to manage labels

  * GHC.Cmm.Parser expands some macros using generators defined in
    this module

  * rts/include/stg/Ticky.h declares all of the global counters

  * rts/include/rts/Ticky.h declares the C data type for an
    STG-declaration's counters

  * some macros defined in rts/include/Cmm.h (and used within the RTS's
    CMM code) update the global ticky counters

  * at the end of execution rts/Ticky.c generates the final report
    +RTS -r<report-file> -RTS

The rts/Ticky.c function that generates the report includes an
STG-declaration's ticky counters if

  * that declaration was entered, or

  * it was allocated (if -ticky-allocd)

On either of those events, the counter is "registered" by adding it to
a linked list; cf the CMM generated by registerTickyCtr.

Ticky-ticky profiling has evolved over many years. Many of the
counters from its most sophisticated days are no longer
active/accurate. As the RTS has changed, sometimes the ticky code for
relevant counters was not accordingly updated. Unfortunately, neither
were the comments.

As of March 2013, there still exist deprecated code and comments in
the code generator as well as the RTS because:

  * I don't know what is out-of-date versus merely commented out for
    momentary convenience, and

  * someone else might know how to repair it!


Note [Ticky counters are static]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Currently GHC only supports static ticky events. That is -ticky emits
code containing labels containing counters which then get bumped at runtime.

There are currently only *static* ticky counters. Either we bump one of the
static counters included in the RTS. Or we emit StgEntCounter structures in
the object code and bump these.
-}

module GHC.StgToCmm.Ticky (
  withNewTickyCounterFun,
  withNewTickyCounterLNE,
  withNewTickyCounterThunk,
  withNewTickyCounterStdThunk,
  withNewTickyCounterCon,
  emitTickyCounterTag,

  tickyDynAlloc,
  tickyAllocHeap,

  tickyAllocPrim,
  tickyAllocThunk,
  tickyAllocPAP,
  tickyHeapCheck,
  tickyStackCheck,

  tickyDirectCall,

  tickyPushUpdateFrame,
  tickyUpdateFrameOmitted,

  tickyEnterDynCon,

  tickyEnterFun,
  tickyEnterThunk,
  tickyEnterLNE,

  tickyUpdateBhCaf,
  tickyUnboxedTupleReturn,
  tickyReturnOldCon, tickyReturnNewCon,

  tickyKnownCallTooFewArgs, tickyKnownCallExact, tickyKnownCallExtraArgs,
  tickySlowCall, tickySlowCallPat,

  tickyTagged, tickyUntagged, tickyTagSkip
  ) where

import GHC.Prelude

import GHC.Platform
import GHC.Platform.Profile

import GHC.StgToCmm.ArgRep    ( slowCallPattern , toArgRep , argRepString )
import GHC.StgToCmm.Closure
import GHC.StgToCmm.Config
import {-# SOURCE #-} GHC.StgToCmm.Foreign   ( emitPrimCall )
import GHC.StgToCmm.Lit       ( newStringCLit )
import GHC.StgToCmm.Monad
import GHC.StgToCmm.Utils

import GHC.Stg.Syntax
import GHC.Cmm.Expr
import GHC.Cmm.Graph
import GHC.Cmm.Utils
import GHC.Cmm.CLabel
import GHC.Runtime.Heap.Layout


import GHC.Types.Name
import GHC.Types.Id
import GHC.Types.Basic
import GHC.Data.FastString
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc
import GHC.Utils.Monad (whenM)

-- Turgid imports for showTypeCategory
import GHC.Builtin.Names
import GHC.Tc.Utils.TcType
import GHC.Core.DataCon
import GHC.Core.TyCon
import GHC.Core.Predicate

import Data.Maybe
import qualified Data.Char
import Control.Monad ( when, unless )
import GHC.Types.Id.Info
import GHC.StgToCmm.Env (getCgInfo_maybe)
import Data.Coerce (coerce)
import GHC.Utils.Json
import GHC.Utils.Unique (anyOfUnique)

-----------------------------------------------------------------------------
--
-- Ticky-ticky profiling
--
-----------------------------------------------------------------------------

-- | Number of arguments for a ticky counter.
--
-- Ticky currently treats args to constructor allocations differently than those for functions/LNE bindings.
tickyArgArity :: TickyClosureType -> Int
tickyArgArity (TickyFun _ _fvs args) = length args
tickyArgArity (TickyLNE args) = length args
tickyArgArity (TickyCon{}) = 0
tickyArgArity (TickyThunk{}) = 0

tickyArgDesc :: TickyClosureType -> String
tickyArgDesc arg_info =
  case arg_info of
    TickyFun _ _fvs args -> map (showTypeCategory . idType . fromNonVoid) args
    TickyLNE args -> map (showTypeCategory . idType . fromNonVoid) args
    TickyThunk{} -> ""
    TickyCon{} -> ""

tickyFvDesc :: TickyClosureType -> String
tickyFvDesc arg_info =
  case arg_info of
    TickyFun _ fvs _args -> map (showTypeCategory . idType . fromNonVoid) fvs
    TickyLNE{} -> ""
    TickyThunk _ _ fvs -> map (showTypeCategory . stgArgType) fvs
    TickyCon{} -> ""

instance ToJson TickyClosureType where
    json info = case info of
      (TickyFun {})   -> mkInfo (tickyFvDesc info) (tickyArgDesc info) "fun"
      (TickyLNE {})   -> mkInfo []                 (tickyArgDesc info) "lne"
      (TickyThunk uf _ _) -> mkInfo (tickyFvDesc info) []              ("thk" ++ if uf then "_u" else "")
      (TickyCon{})    -> mkInfo []                 []                  "con"
      where
        mkInfo :: String -> String -> String -> JsonDoc
        mkInfo fvs args ty =
          JSObject
              [("type", json "entCntr")
              ,("subTy", json ty)
              ,("fvs_c", json (length fvs))
              ,("fvs" , json fvs)
              ,("args", json args)
              ]

tickyEntryDescJson :: (SDocContext -> TickyClosureType -> String)
tickyEntryDescJson ctxt = renderWithContext ctxt . renderJSON . json

data TickyClosureType
    = TickyFun
        Bool -- True <-> single entry
        [NonVoid Id] -- ^ FVs
        [NonVoid Id] -- ^ Args
    | TickyCon
        DataCon -- the allocated constructor
        ConstructorNumber
    | TickyThunk
        Bool -- True <-> updateable
        Bool -- True <-> standard thunk (AP or selector), has no entry counter
        [StgArg] -- ^ FVS, StgArg because for thunks these can also be literals.
    | TickyLNE
        [NonVoid Id] -- ^ Args

withNewTickyCounterFun :: Bool -> Id -> [NonVoid Id] -> [NonVoid Id] -> FCode a -> FCode a
withNewTickyCounterFun single_entry f fvs args = withNewTickyCounter (TickyFun single_entry fvs args) f

withNewTickyCounterLNE :: Id  ->  [NonVoid Id] -> FCode a -> FCode a
withNewTickyCounterLNE nm args code = do
  b <- isEnabled stgToCmmTickyLNE
  if not b then code else withNewTickyCounter (TickyLNE args) nm code

thunkHasCounter :: Bool -> FCode Bool
thunkHasCounter isStatic = (not isStatic &&) <$> isEnabled stgToCmmTickyDynThunk

withNewTickyCounterThunk
  :: Bool -- ^ static
  -> Bool -- ^ updateable
  -> Id
  -> [NonVoid Id] -- ^ Free vars
  -> FCode a
  -> FCode a
withNewTickyCounterThunk isStatic isUpdatable name fvs code = do
    has_ctr <- thunkHasCounter isStatic
    if not has_ctr
      then code
      else withNewTickyCounter (TickyThunk isUpdatable False (map StgVarArg $ coerce fvs)) name code

withNewTickyCounterStdThunk
  :: Bool -- ^ updateable
  -> Id
  -> [StgArg] -- ^ Free vars + function
  -> FCode a
  -> FCode a
withNewTickyCounterStdThunk isUpdatable name fvs code = do
    has_ctr <- thunkHasCounter False
    if not has_ctr
      then code
      else withNewTickyCounter (TickyThunk isUpdatable True fvs) name code

withNewTickyCounterCon
  :: Id
  -> DataCon
  -> ConstructorNumber
  -> FCode a
  -> FCode a
withNewTickyCounterCon name datacon info code = do
    has_ctr <- thunkHasCounter False
    if not has_ctr
      then code
      else withNewTickyCounter (TickyCon datacon info) name code

-- args does not include the void arguments
withNewTickyCounter :: TickyClosureType -> Id -> FCode a -> FCode a
withNewTickyCounter cloType name m = do
  lbl <- emitTickyCounter cloType name
  setTickyCtrLabel lbl m

emitTickyData :: Platform
              -> CLabel -- ^ lbl for the counter
              -> Arity -- ^ arity
              -> CmmLit -- ^ fun desc
              -> CmmLit -- ^ arg desc
              -> CmmLit -- ^ json desc
              -> CmmLit -- ^ info table lbl
              -> FCode ()
emitTickyData platform ctr_lbl arity fun_desc arg_desc json_desc info_tbl =
  emitDataLits ctr_lbl
    -- Must match layout of rts/include/rts/Ticky.h's StgEntCounter
    --
    -- krc: note that all the fields are I32 now; some were I16
    -- before, but the code generator wasn't handling that
    -- properly and it led to chaos, panic and disorder.
        [ zeroCLit platform,               -- registered?
          mkIntCLit platform arity,   -- Arity
          zeroCLit platform,               -- Heap allocated for this thing
          fun_desc,
          arg_desc,
          json_desc,
          info_tbl,
          zeroCLit platform,          -- Entries into this thing
          zeroCLit platform,          -- Heap allocated by this thing
          zeroCLit platform           -- Link to next StgEntCounter
        ]


emitTickyCounter :: TickyClosureType -> Id -> FCode CLabel
emitTickyCounter cloType tickee
  = let name = idName tickee in
    let ctr_lbl = mkRednCountsLabel name in
    (>> return ctr_lbl) $
    ifTicky $ do
        { cfg    <- getStgToCmmConfig
        ; parent <- getTickyCtrLabel
        ; mod_name <- getModuleName

          -- When printing the name of a thing in a ticky file, we
          -- want to give the module name even for *local* things.  We
          -- print just "x (M)" rather that "M.x" to distinguish them
          -- from the global kind by calling to @pprTickyName@
        ; let platform = stgToCmmPlatform cfg
              ppr_for_ticky_name :: SDoc
              ppr_for_ticky_name =
                let ext = case cloType of
                              TickyFun single_entry _ _-> parens $ hcat $ punctuate comma $
                                  [text "fun"] ++ [text "se"|single_entry]
                              TickyCon datacon _cn -> parens (text "con:" <+> ppr (dataConName datacon))
                              TickyThunk upd std _-> parens $ hcat $ punctuate comma $
                                  [text "thk"] ++ [text "se"|not upd] ++ [text "std"|std]
                              TickyLNE _ | isInternalName name -> parens (text "LNE")
                                         | otherwise -> panic "emitTickyCounter: how is this an external LNE?"
                    p = case hasHaskellName parent of
                            -- NB the default "top" ticky ctr does not
                            -- have a Haskell name
                          Just pname -> text "in" <+> ppr (nameUnique pname)
                          _ -> empty
                in pprTickyName mod_name name <+> ext <+> p
        ; this_mod <- getModuleName
        ; let t = case cloType of
                    TickyCon {} -> "C"
                    TickyFun {} -> "F"
                    TickyThunk {} -> "T"
                    TickyLNE {} -> "L"
        ; info_lbl <- case cloType of
                            TickyCon dc mn -> case mn of
                                               NoNumber -> return $! CmmLabel $ mkConInfoTableLabel (dataConName dc) DefinitionSite
                                               (Numbered n) -> return $! CmmLabel $ mkConInfoTableLabel (dataConName dc) (UsageSite this_mod n)
                            TickyFun {} ->
                              return $! CmmLabel $ mkInfoTableLabel name NoCafRefs

                            TickyThunk _ std_thunk _fvs
                              | not std_thunk
                              -> return $! CmmLabel $ mkInfoTableLabel name NoCafRefs
                              -- IPE Maps have no entry for std thunks.
                              | otherwise
                              -> do
                                    lf_info <- getCgInfo_maybe name
                                    profile <- getProfile
                                    case lf_info of
                                      Just (CgIdInfo { cg_lf = cg_lf })
                                          | isLFThunk cg_lf
                                          -> return $! CmmLabel $ mkClosureInfoTableLabel (profilePlatform profile) tickee cg_lf
                                      _   -> pprTraceDebug "tickyThunkUnknown" (text t <> colon <> ppr name <+> pprDebugCLabel (profilePlatform profile) (mkInfoTableLabel name NoCafRefs))
                                            return $! zeroCLit platform

                            TickyLNE {} -> return $! zeroCLit platform

        ; let ctx = defaultSDocContext {sdocPprDebug = True}
        ; fun_descr_lit <- newStringCLit $ renderWithContext ctx ppr_for_ticky_name
        ; arg_descr_lit <- newStringCLit $ tickyArgDesc cloType
        ; json_descr_lit <- newStringCLit $ tickyEntryDescJson ctx cloType
        ; emitTickyData platform ctr_lbl (tickyArgArity cloType) fun_descr_lit arg_descr_lit json_descr_lit info_lbl
        }

{- Note [TagSkip ticky counters]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
These counters keep track how often we execute code where we
would have performed a tag check if we hadn't run tag inference.

If we have some code of the form:
    case v[tagged] of ...
and we want to record how often we avoid a tag check on v
through tag inference we have to emit a new StgEntCounter for
each such case statement in order to record how often it's executed.

In theory we could emit one per *binding*. But then we
would have to either keep track of the bindings which
already have a StgEntCounter associated with them in the
code gen state or preallocate such a structure for each binding
in the code unconditionally (since ticky-code can call non-ticky code)

The first makes the compiler slower, even when ticky is not
used (a big no no). The later is fairly complex but increases code size
unconditionally. See also Note [Ticky counters are static].

So instead we emit a new StgEntCounter for each use site of a binding
where we infered a tag to be present. And increment the counter whenever
this use site is executed.

We use the fields as follows:

entry_count: Entries avoided.
str:       : Name of the id.

We use emitTickyCounterTag to emit the counter.

Unlike the closure counters each *use* site of v has it's own
counter. So there is no need to keep track of the closure/case we are
in.

We also have to pass a unique for the counter. An Id might be
scrutinized in more than one place, so the ID alone isn't enough
to distinguish between use sites.
-}

emitTickyCounterTag :: Unique -> NonVoid Id -> FCode CLabel
emitTickyCounterTag unique (NonVoid id) =
  let name = idName id
      ctr_lbl = mkTagHitLabel name unique in
    (>> return ctr_lbl) $
    ifTickyTag $ do
        { platform <- getPlatform
        ; parent <- getTickyCtrLabel
        ; mod_name <- getModuleName

          -- When printing the name of a thing in a ticky file, we
          -- want to give the module name even for *local* things.  We
          -- print just "x (M)" rather that "M.x" to distinguish them
          -- from the global kind.
        ; let ppr_for_ticky_name :: SDoc
              ppr_for_ticky_name =
                let n = ppr name
                    ext = empty -- parens (text "tagged")
                    p = case hasHaskellName parent of
                            -- NB the default "top" ticky ctr does not
                            -- have a Haskell name
                          Just pname -> text "at" <+> ppr (nameSrcLoc pname) <+>
                                          text "in" <+> pprNameUnqualified name
                          _ -> empty
                in if isInternalName name
                   then n <+> parens (ppr mod_name) <+> ext <+> p
                   else n <+> ext <+> p
        ; sdoc_context <- stgToCmmContext <$> getStgToCmmConfig
        ; fun_descr_lit <- newStringCLit $ renderWithContext sdoc_context ppr_for_ticky_name
        ; arg_descr_lit <- newStringCLit $ "infer"
        ; json_descr_lit <- newStringCLit $ "infer"
        ; emitTickyData platform ctr_lbl 0 fun_descr_lit arg_descr_lit json_descr_lit (zeroCLit platform)
        }
-- -----------------------------------------------------------------------------
-- Ticky stack frames

tickyPushUpdateFrame, tickyUpdateFrameOmitted :: FCode ()
tickyPushUpdateFrame    = ifTicky $ bumpTickyCounter (fsLit "UPDF_PUSHED_ctr")
tickyUpdateFrameOmitted = ifTicky $ bumpTickyCounter (fsLit "UPDF_OMITTED_ctr")

-- -----------------------------------------------------------------------------
-- Ticky entries

-- NB the name-specific entries are only available for names that have
-- dedicated Cmm code. As far as I know, this just rules out
-- constructor thunks. For them, there is no CMM code block to put the
-- bump of name-specific ticky counter into. On the other hand, we can
-- still track allocation their allocation.

tickyEnterDynCon :: FCode ()
tickyEnterDynCon = ifTicky $ bumpTickyCounter (fsLit "ENT_DYN_CON_ctr")

tickyEnterThunk :: ClosureInfo -> FCode ()
tickyEnterThunk cl_info
  = ifTicky $ do
    { bumpTickyCounter ctr
    ; has_ctr <- thunkHasCounter static
    ; when has_ctr $ do
      ticky_ctr_lbl <- getTickyCtrLabel
      registerTickyCtrAtEntryDyn ticky_ctr_lbl
      bumpTickyEntryCount ticky_ctr_lbl }
  where
    updatable = not (closureUpdReqd cl_info)
    static    = isStaticClosure cl_info

    ctr | static    = if updatable then fsLit "ENT_STATIC_THK_SINGLE_ctr"
                                   else fsLit "ENT_STATIC_THK_MANY_ctr"
        | otherwise = if updatable then fsLit "ENT_DYN_THK_SINGLE_ctr"
                                   else fsLit "ENT_DYN_THK_MANY_ctr"

tickyUpdateBhCaf :: ClosureInfo -> FCode ()
tickyUpdateBhCaf cl_info
  = ifTicky (bumpTickyCounter ctr)
  where
    ctr | closureUpdReqd cl_info = (fsLit "UPD_CAF_BH_SINGLE_ENTRY_ctr")
        | otherwise              = (fsLit "UPD_CAF_BH_UPDATABLE_ctr")

tickyEnterFun :: ClosureInfo -> FCode ()
tickyEnterFun cl_info = ifTicky $ do
  ctr_lbl <- getTickyCtrLabel

  if isStaticClosure cl_info
    then do bumpTickyCounter (fsLit "ENT_STATIC_FUN_DIRECT_ctr")
            registerTickyCtr ctr_lbl
    else do bumpTickyCounter (fsLit "ENT_DYN_FUN_DIRECT_ctr")
            registerTickyCtrAtEntryDyn ctr_lbl

  bumpTickyEntryCount ctr_lbl

tickyEnterLNE :: FCode ()
tickyEnterLNE = ifTicky $ do
  bumpTickyCounter (fsLit "ENT_LNE_ctr")
  ifTickyLNE $ do
    ctr_lbl <- getTickyCtrLabel
    registerTickyCtr ctr_lbl
    bumpTickyEntryCount ctr_lbl

-- needn't register a counter upon entry if
--
-- 1) it's for a dynamic closure, and
--
-- 2) -ticky-allocd is on
--
-- since the counter was registered already upon being alloc'd
registerTickyCtrAtEntryDyn :: CLabel -> FCode ()
registerTickyCtrAtEntryDyn ctr_lbl = do
  already_registered <- isEnabled stgToCmmTickyAllocd
  unless already_registered $ registerTickyCtr ctr_lbl

-- | Register a ticky counter.
--
-- It's important that this does not race with other entries of the same
-- closure, lest the ticky_entry_ctrs list may become cyclic. However, we also
-- need to make sure that this is reasonably efficient. Consequently, we first
-- perform a normal load of the counter's "registered" flag to check whether
-- registration is necessary. If so, then we do a compare-and-swap to lock the
-- counter for registration and use an atomic-exchange to add the counter to the list.
--
-- @
-- if ( f_ct.registeredp == 0 ) {
--    if (cas(f_ct.registeredp, 0, 1) == 0) {
--        old_head = xchg(ticky_entry_ctrs,  f_ct);
--        f_ct.link = old_head;
--    }
-- }
-- @
registerTickyCtr :: CLabel -> FCode ()
registerTickyCtr ctr_lbl = do
  platform <- getPlatform
  let constants = platformConstants platform
      word_width = wordWidth platform
      registeredp = CmmLit (cmmLabelOffB ctr_lbl (pc_OFFSET_StgEntCounter_registeredp constants))

  register_stmts <- getCode $ do
    old_head <- newTemp (bWord platform)
    let ticky_entry_ctrs = mkLblExpr (mkRtsCmmDataLabel (fsLit "ticky_entry_ctrs"))
        link = CmmLit (cmmLabelOffB ctr_lbl (pc_OFFSET_StgEntCounter_link constants))
    emitPrimCall [old_head] (MO_Xchg word_width) [ticky_entry_ctrs, mkLblExpr ctr_lbl]
    emitStore link (CmmReg $ CmmLocal old_head)

  cas_test <- getCode $ do
    old <- newTemp (bWord platform)
    emitPrimCall [old] (MO_Cmpxchg word_width)
        [registeredp, zeroExpr platform, mkIntExpr platform 1]
    let locked = cmmEqWord platform (CmmReg $ CmmLocal old) (zeroExpr platform)
    emit =<< mkCmmIfThen locked register_stmts

  let test = cmmEqWord platform (cmmLoadBWord platform registeredp) (zeroExpr platform)
  emit =<< mkCmmIfThen test cas_test

tickyReturnOldCon, tickyReturnNewCon :: RepArity -> FCode ()
tickyReturnOldCon arity
  = ifTicky $ do { bumpTickyCounter (fsLit "RET_OLD_ctr")
                 ; bumpHistogram    (fsLit "RET_OLD_hst") arity }
tickyReturnNewCon arity
  = ifTicky $ do { bumpTickyCounter (fsLit "RET_NEW_ctr")
                 ; bumpHistogram    (fsLit "RET_NEW_hst") arity }

tickyUnboxedTupleReturn :: RepArity -> FCode ()
tickyUnboxedTupleReturn arity
  = ifTicky $ do { bumpTickyCounter (fsLit "RET_UNBOXED_TUP_ctr")
                 ; bumpHistogram    (fsLit "RET_UNBOXED_TUP_hst") arity }

-- -----------------------------------------------------------------------------
-- Ticky calls

-- Ticks at a *call site*:
tickyDirectCall :: RepArity -> [StgArg] -> FCode ()
tickyDirectCall arity args
  | args `lengthIs` arity = tickyKnownCallExact
  | otherwise = do tickyKnownCallExtraArgs
                   tickySlowCallPat (drop arity args)

tickyKnownCallTooFewArgs :: FCode ()
tickyKnownCallTooFewArgs = ifTicky $ bumpTickyCounter (fsLit "KNOWN_CALL_TOO_FEW_ARGS_ctr")

tickyKnownCallExact :: FCode ()
tickyKnownCallExact      = ifTicky $ bumpTickyCounter (fsLit "KNOWN_CALL_ctr")

tickyKnownCallExtraArgs :: FCode ()
tickyKnownCallExtraArgs  = ifTicky $ bumpTickyCounter (fsLit "KNOWN_CALL_EXTRA_ARGS_ctr")

tickyUnknownCall :: FCode ()
tickyUnknownCall         = ifTicky $ bumpTickyCounter (fsLit "UNKNOWN_CALL_ctr")

-- Tick for the call pattern at slow call site (i.e. in addition to
-- tickyUnknownCall, tickyKnownCallExtraArgs, etc.)
tickySlowCall :: LambdaFormInfo -> [StgArg] -> FCode ()
tickySlowCall _ [] = return ()
tickySlowCall lf_info args = do
 -- see Note [Ticky for slow calls]
 if isKnownFun lf_info
   then tickyKnownCallTooFewArgs
   else tickyUnknownCall
 tickySlowCallPat args

tickySlowCallPat :: [StgArg] -> FCode ()
tickySlowCallPat args = ifTicky $ do
  platform <- profilePlatform <$> getProfile
  let argReps = map (toArgRep platform . stgArgRep1) args
      (_, n_matched) = slowCallPattern argReps
  if n_matched > 0 && args `lengthIs` n_matched
     then bumpTickyLbl $ mkRtsSlowFastTickyCtrLabel $ concatMap (map Data.Char.toLower . argRepString) argReps
     else bumpTickyCounter $ fsLit "VERY_SLOW_CALL_ctr"

{-

Note [Ticky for slow calls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Terminology is unfortunately a bit mixed up for these calls. codeGen
uses "slow call" to refer to unknown calls and under-saturated known
calls.

Nowadays, though (ie as of the eval/apply paper), the significantly
slower calls are actually just a subset of these: the ones with no
built-in argument pattern (cf GHC.StgToCmm.ArgRep.slowCallPattern)

So for ticky profiling, we split slow calls into
"SLOW_CALL_fast_<pattern>_ctr" (those matching a built-in pattern) and
VERY_SLOW_CALL_ctr (those without a built-in pattern; these are very
bad for both space and time).

-}

-- -----------------------------------------------------------------------------
-- Ticky allocation

tickyDynAlloc :: Maybe Id -> SMRep -> LambdaFormInfo -> FCode ()
-- Called when doing a dynamic heap allocation; the LambdaFormInfo
-- used to distinguish between closure types
--
-- TODO what else to count while we're here?
tickyDynAlloc mb_id rep lf = ifTicky $ do
  profile <- getProfile
  let platform = profilePlatform profile
      bytes = platformWordSizeInBytes platform * heapClosureSizeW profile rep

      countGlobal tot ctr = do
        bumpTickyCounterBy tot bytes
        bumpTickyCounter   ctr
      countSpecific = ifTickyAllocd $ case mb_id of
        Nothing -> return ()
        Just id -> do
          let ctr_lbl = mkRednCountsLabel (idName id)
          registerTickyCtr ctr_lbl
          bumpTickyAllocd ctr_lbl bytes

  -- TODO are we still tracking "good stuff" (_gds) versus
  -- administrative (_adm) versus slop (_slp)? I'm going with all _gds
  -- for now, since I don't currently know neither if we do nor how to
  -- distinguish. NSF Mar 2013

  if | isConRep rep   ->
         ifTickyDynThunk countSpecific >>
         countGlobal (fsLit "ALLOC_CON_gds") (fsLit "ALLOC_CON_ctr")
     | isThunkRep rep ->
         ifTickyDynThunk countSpecific >>
         if lfUpdatable lf
         then countGlobal (fsLit "ALLOC_THK_gds") (fsLit "ALLOC_UP_THK_ctr")
         else countGlobal (fsLit "ALLOC_THK_gds") (fsLit "ALLOC_SE_THK_ctr")
     | isFunRep   rep ->
         countSpecific >>
         countGlobal (fsLit "ALLOC_FUN_gds") (fsLit "ALLOC_FUN_ctr")
     | otherwise      -> panic "How is this heap object not a con, thunk, or fun?"



tickyAllocHeap ::
  Bool -> -- is this a genuine allocation? As opposed to
          -- GHC.StgToCmm.Layout.adjustHpBackwards
  VirtualHpOffset -> FCode ()
-- Called when doing a heap check [TICK_ALLOC_HEAP]
-- Must be lazy in the amount of allocation!
tickyAllocHeap genuine hp
  = ifTicky $
    do  { platform <- getPlatform
        ; ticky_ctr <- getTickyCtrLabel
        ; emit $ catAGraphs $
            -- only test hp from within the emit so that the monadic
            -- computation itself is not strict in hp (cf knot in
            -- GHC.StgToCmm.Monad.getHeapUsage)
          if hp == 0 then []
          else let !bytes = platformWordSizeInBytes platform * hp in [
            -- Bump the allocation total in the closure's StgEntCounter
            addToMem (rEP_StgEntCounter_allocs platform)
                     (CmmLit (cmmLabelOffB ticky_ctr (pc_OFFSET_StgEntCounter_allocs (platformConstants platform))))
                     bytes,
            -- Bump the global allocation total ALLOC_HEAP_tot
            addToMemLbl (bWord platform)
                        (mkRtsCmmDataLabel (fsLit "ALLOC_HEAP_tot"))
                        bytes,
            -- Bump the global allocation counter ALLOC_HEAP_ctr
            if not genuine then mkNop
            else addToMemLbl (bWord platform)
                             (mkRtsCmmDataLabel (fsLit "ALLOC_HEAP_ctr"))
                             1
            ]}


--------------------------------------------------------------------------------
-- these three are only called from GHC.Cmm.Parser (ie ultimately from the RTS)

-- the units are bytes

tickyAllocPrim :: CmmExpr  -- ^ size of the full header, in bytes
               -> CmmExpr  -- ^ size of the payload, in bytes
               -> CmmExpr -> FCode ()
tickyAllocPrim _hdr _goods _slop = ifTicky $ do
  bumpTickyCounter    (fsLit "ALLOC_PRIM_ctr")
  bumpTickyCounterByE (fsLit "ALLOC_PRIM_adm") _hdr
  bumpTickyCounterByE (fsLit "ALLOC_PRIM_gds") _goods
  bumpTickyCounterByE (fsLit "ALLOC_PRIM_slp") _slop

tickyAllocThunk :: CmmExpr -> CmmExpr -> FCode ()
tickyAllocThunk _goods _slop = ifTicky $ do
    -- TODO is it ever called with a Single-Entry thunk?
  bumpTickyCounter    (fsLit "ALLOC_UP_THK_ctr")
  bumpTickyCounterByE (fsLit "ALLOC_THK_gds") _goods
  bumpTickyCounterByE (fsLit "ALLOC_THK_slp") _slop

tickyAllocPAP :: CmmExpr -> CmmExpr -> FCode ()
tickyAllocPAP _goods _slop = ifTicky $ do
  bumpTickyCounter    (fsLit "ALLOC_PAP_ctr")
  bumpTickyCounterByE (fsLit "ALLOC_PAP_gds") _goods
  bumpTickyCounterByE (fsLit "ALLOC_PAP_slp") _slop

tickyHeapCheck :: FCode ()
tickyHeapCheck = ifTicky $ bumpTickyCounter (fsLit "HEAP_CHK_ctr")

tickyStackCheck :: FCode ()
tickyStackCheck = ifTicky $ bumpTickyCounter (fsLit "STK_CHK_ctr")

-- -----------------------------------------------------------------------------
-- Ticky for tag inference characterisation

-- | Predicted a pointer would be tagged correctly (GHC will crash if not so no miss case)
tickyTagged :: FCode ()
tickyTagged         = ifTickyTag $ bumpTickyCounter (fsLit "TAG_TAGGED_pred")

-- | Pass a boolean expr indicating if tag was present.
tickyUntagged :: CmmExpr -> FCode ()
tickyUntagged e     = do
    ifTickyTag $ bumpTickyCounter (fsLit "TAG_UNTAGGED_pred")
    ifTickyTag $ bumpTickyCounterByE (fsLit "TAG_UNTAGGED_miss") e

-- | Called when for `case v of ...` we can avoid entering v based on
-- tag inference information.
tickyTagSkip :: Unique -> Id -> FCode ()
tickyTagSkip unique id = ifTickyTag $ do
  let ctr_lbl = mkTagHitLabel (idName id) unique
  registerTickyCtr ctr_lbl
  bumpTickyTagSkip ctr_lbl

-- -----------------------------------------------------------------------------
-- Ticky utils

isEnabled :: (StgToCmmConfig -> Bool) -> FCode Bool
isEnabled = flip fmap getStgToCmmConfig

runIfFlag :: (StgToCmmConfig -> Bool) -> FCode () -> FCode ()
runIfFlag f = whenM (f <$> getStgToCmmConfig)

ifTicky :: FCode () -> FCode ()
ifTicky = runIfFlag stgToCmmDoTicky

ifTickyTag :: FCode () -> FCode ()
ifTickyTag = runIfFlag stgToCmmTickyTag

ifTickyAllocd :: FCode () -> FCode ()
ifTickyAllocd = runIfFlag stgToCmmTickyAllocd

ifTickyLNE :: FCode () -> FCode ()
ifTickyLNE = runIfFlag stgToCmmTickyLNE

ifTickyDynThunk :: FCode () -> FCode ()
ifTickyDynThunk = runIfFlag stgToCmmTickyDynThunk

bumpTickyCounter :: FastString -> FCode ()
bumpTickyCounter = bumpTickyLbl . mkRtsCmmDataLabel

bumpTickyCounterBy :: FastString -> Int -> FCode ()
bumpTickyCounterBy = bumpTickyLblBy . mkRtsCmmDataLabel

bumpTickyCounterByE :: FastString -> CmmExpr -> FCode ()
bumpTickyCounterByE lbl = bumpTickyLblByE (mkRtsCmmDataLabel lbl)

bumpTickyEntryCount :: CLabel -> FCode ()
bumpTickyEntryCount lbl = do
  platform <- getPlatform
  bumpTickyLit (cmmLabelOffB lbl (pc_OFFSET_StgEntCounter_entry_count (platformConstants platform)))

bumpTickyAllocd :: CLabel -> Int -> FCode ()
bumpTickyAllocd lbl bytes = do
  platform <- getPlatform
  bumpTickyLitBy (cmmLabelOffB lbl (pc_OFFSET_StgEntCounter_entry_count (platformConstants platform))) bytes

bumpTickyTagSkip :: CLabel -> FCode ()
bumpTickyTagSkip lbl = do
  platform <- getPlatform
  bumpTickyLitBy (cmmLabelOffB lbl (pc_OFFSET_StgEntCounter_entry_count (platformConstants platform))) 1

bumpTickyLbl :: CLabel -> FCode ()
bumpTickyLbl lhs = bumpTickyLitBy (cmmLabelOffB lhs 0) 1

bumpTickyLblBy :: CLabel -> Int -> FCode ()
bumpTickyLblBy lhs = bumpTickyLitBy (cmmLabelOffB lhs 0)

bumpTickyLblByE :: CLabel -> CmmExpr -> FCode ()
bumpTickyLblByE lhs = bumpTickyLitByE (cmmLabelOffB lhs 0)

bumpTickyLit :: CmmLit -> FCode ()
bumpTickyLit lhs = bumpTickyLitBy lhs 1

bumpTickyLitBy :: CmmLit -> Int -> FCode ()
bumpTickyLitBy lhs n = do
  platform <- getPlatform
  emit (addToMem (bWord platform) (CmmLit lhs) n)

bumpTickyLitByE :: CmmLit -> CmmExpr -> FCode ()
bumpTickyLitByE lhs e = do
  platform <- getPlatform
  emit (addToMemE (bWord platform) (CmmLit lhs) e)

bumpHistogram :: FastString -> Int -> FCode ()
bumpHistogram lbl n = do
    platform <- getPlatform
    let offset = n `min` (pc_TICKY_BIN_COUNT (platformConstants platform) - 1)
    emit (addToMem (bWord platform)
           (cmmIndexExpr platform
                (wordWidth platform)
                (CmmLit (CmmLabel (mkRtsCmmDataLabel lbl)))
                (CmmLit (CmmInt (fromIntegral offset) (wordWidth platform))))
           1)

------------------------------------------------------------------
-- Showing the "type category" for ticky-ticky profiling

showTypeCategory :: Type -> Char
  {-
        +           dictionary

        >           function

        {C,I,F,D,W} char, int, float, double, word
        {c,i,f,d,w} unboxed ditto

        T           tuple

        P           other primitive type
        p           unboxed ditto

        L           list
        E           enumeration type
        S           other single-constructor type
        M           other multi-constructor data-con type

        .           other type

        -           reserved for others to mark as "uninteresting"

  Accurate as of Mar 2013, but I eliminated the Array category instead
  of updating it, for simplicity. It's in P/p, I think --NSF

    -}
showTypeCategory ty
  | isDictTy ty = '+'
  | otherwise = case tcSplitTyConApp_maybe ty of
  Nothing -> '.'
  Just (tycon, _) ->
    case () of
      _ | anyOfUnique tycon [fUNTyConKey] -> '>'
        | anyOfUnique tycon [charTyConKey] -> 'C'
        | anyOfUnique tycon [charPrimTyConKey] -> 'c'
        | anyOfUnique tycon [doubleTyConKey] -> 'D'
        | anyOfUnique tycon [doublePrimTyConKey] -> 'd'
        | anyOfUnique tycon [floatTyConKey] -> 'F'
        | anyOfUnique tycon [floatPrimTyConKey] -> 'f'
        | anyOfUnique tycon [intTyConKey, int8TyConKey, int16TyConKey, int32TyConKey, int64TyConKey] -> 'I'
        | anyOfUnique tycon [intPrimTyConKey, int8PrimTyConKey, int16PrimTyConKey, int32PrimTyConKey, int64PrimTyConKey] -> 'i'
        | anyOfUnique tycon [wordTyConKey, word8TyConKey, word16TyConKey, word32TyConKey, word64TyConKey] -> 'W'
        | anyOfUnique tycon [wordPrimTyConKey, word8PrimTyConKey, word16PrimTyConKey, word32PrimTyConKey, word64PrimTyConKey] -> 'w'
        | anyOfUnique tycon [listTyConKey] -> 'L'
        | isUnboxedTupleTyCon tycon -> 't'
        | isTupleTyCon tycon       -> 'T'
        | isPrimTyCon tycon        -> 'P'
        | isEnumerationTyCon tycon -> 'E'
        | isJust (tyConSingleDataCon_maybe tycon) -> 'S'
        | otherwise -> 'M' -- oh, well...

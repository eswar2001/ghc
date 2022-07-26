{-
(c) The AQUA Project, Glasgow University, 1993-1998

-}

{-# LANGUAGE DeriveFunctor #-}

module GHC.Plugins.Monad (
    -- * Plugins
    bindsOnlyPass,

    -- * The monad
    CoreM, runCoreM, unliftCoreM,

    mapDynFlagsCoreM,

    -- ** Reading from the monad
    getHscEnv, getModule,
    getRuleBase, getExternalRuleBase,
    getDynFlags, getPackageFamInstEnv,
    getInteractiveContext,
    getVisibleOrphanMods, getUniqMask,
    getPrintUnqualified, getSrcSpanM,

    -- ** Writing to the monad
    addSimplCount, dropSimplCount,

    -- ** Lifting into the monad
    liftIO, liftIOWithCount,

    -- ** Dealing with annotations
    getAnnotations, getFirstAnnotations,

    -- ** Screen output
    putMsg, putMsgS, errorMsg, errorMsgS, msg,
    fatalErrorMsg, fatalErrorMsgS,
    debugTraceMsg, debugTraceMsgS,
  ) where

import GHC.Prelude hiding ( read )

import GHC.Driver.Session
import GHC.Driver.Env

import GHC.Core
import GHC.Core.Opt.Stats ( SimplCount, zeroSimplCount, plusSimplCount )
import GHC.Core.Opt.Utils

import GHC.Runtime.Context ( InteractiveContext )

import GHC.Types.Unique.Supply
import GHC.Types.Name.Env
import GHC.Types.SrcLoc
import GHC.Types.Error

import GHC.Utils.Error ( errorDiagnostic )
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Logger
import GHC.Utils.Monad

import GHC.Data.IOEnv hiding     ( liftIO, failM, failWithM )
import qualified GHC.Data.IOEnv  as IOEnv

import GHC.Unit.Module
import GHC.Unit.Module.ModGuts
import GHC.Unit.External

import Data.Typeable ( Typeable )
import Data.Word
import Control.Monad
import Control.Applicative ( Alternative(..) )

{-
************************************************************************
*                                                                      *
          Abstraction of core-to-core passes to run.
*                                                                      *
************************************************************************
-}

bindsOnlyPass :: (CoreProgram -> CoreM CoreProgram) -> ModGuts -> CoreM ModGuts
bindsOnlyPass pass guts
  = do { binds' <- pass (mg_binds guts)
       ; return (guts { mg_binds = binds' }) }

{-
************************************************************************
*                                                                      *
             Monad and carried data structure definitions
*                                                                      *
************************************************************************
-}

data CoreReader = CoreReader {
        cr_hsc_env             :: HscEnv,
        cr_rule_base           :: RuleBase,
        cr_module              :: Module,
        cr_print_unqual        :: PrintUnqualified,
        cr_loc                 :: SrcSpan,   -- Use this for log/error messages so they
                                             -- are at least tagged with the right source file
        cr_visible_orphan_mods :: !ModuleSet,
        cr_uniq_mask           :: !Char      -- Mask for creating unique values
}

-- Note: CoreWriter used to be defined with data, rather than newtype.  If it
-- is defined that way again, the cw_simpl_count field, at least, must be
-- strict to avoid a space leak (#7702).
newtype CoreWriter = CoreWriter {
        cw_simpl_count :: SimplCount
}

emptyWriter :: Bool -- ^ -ddump-simpl-stats
            -> CoreWriter
emptyWriter dump_simpl_stats = CoreWriter {
        cw_simpl_count = zeroSimplCount dump_simpl_stats
    }

plusWriter :: CoreWriter -> CoreWriter -> CoreWriter
plusWriter w1 w2 = CoreWriter {
        cw_simpl_count = (cw_simpl_count w1) `plusSimplCount` (cw_simpl_count w2)
    }

type CoreIOEnv = IOEnv CoreReader

-- | The monad used by Core plugin passes to register simplification statistics.
--  Also used to have common state (in the form of UniqueSupply) for generating Uniques.
newtype CoreM a = CoreM { unCoreM :: CoreIOEnv (a, CoreWriter) }
    deriving (Functor)

instance Monad CoreM where
    mx >>= f = CoreM $ do
            (x, w1) <- unCoreM mx
            (y, w2) <- unCoreM (f x)
            let w = w1 `plusWriter` w2
            return $ seq w (y, w)
            -- forcing w before building the tuple avoids a space leak
            -- (#7702)

instance Applicative CoreM where
    pure x = CoreM $ nop x
    (<*>) = ap
    m *> k = m >>= \_ -> k

instance Alternative CoreM where
    empty   = CoreM Control.Applicative.empty
    m <|> n = CoreM (unCoreM m <|> unCoreM n)

instance MonadPlus CoreM

instance MonadUnique CoreM where
    getUniqueSupplyM = do
        mask <- read cr_uniq_mask
        liftIO $! mkSplitUniqSupply mask

    getUniqueM = do
        mask <- read cr_uniq_mask
        liftIO $! uniqFromMask mask

runCoreM :: HscEnv
         -> RuleBase
         -> Char -- ^ Mask
         -> Module
         -> ModuleSet
         -> PrintUnqualified
         -> SrcSpan
         -> CoreM a
         -> IO (a, SimplCount)
runCoreM hsc_env rule_base mask mod orph_imps print_unqual loc m
  = liftM extract $ runIOEnv reader $ unCoreM m
  where
    reader = CoreReader {
            cr_hsc_env = hsc_env,
            cr_rule_base = rule_base,
            cr_module = mod,
            cr_visible_orphan_mods = orph_imps,
            cr_print_unqual = print_unqual,
            cr_loc = loc,
            cr_uniq_mask = mask
        }

    extract :: (a, CoreWriter) -> (a, SimplCount)
    extract (value, writer) = (value, cw_simpl_count writer)

unliftCoreM :: ((CoreM a -> IO (a, SimplCount)) -> IO r) -> CoreM r
unliftCoreM f = do
  reader <- read id
  liftIO $ f (liftM extract . runIOEnv reader . unCoreM)
  where
    extract :: (a, CoreWriter) -> (a, SimplCount)
    extract (value, writer) = (value, cw_simpl_count writer)

{-
************************************************************************
*                                                                      *
             Core combinators, not exported
*                                                                      *
************************************************************************
-}

nop :: a -> CoreIOEnv (a, CoreWriter)
nop x = do
    logger <- hsc_logger . cr_hsc_env <$> getEnv
    return (x, emptyWriter $ logHasDumpFlag logger Opt_D_dump_simpl_stats)

read :: (CoreReader -> a) -> CoreM a
read f = CoreM $ getEnv >>= (\r -> nop (f r))

write :: CoreWriter -> CoreM ()
write w = CoreM $ return ((), w)

-- \subsection{Lifting IO into the monad}

-- | Lift an 'IOEnv' operation into 'CoreM'
liftIOEnv :: CoreIOEnv a -> CoreM a
liftIOEnv mx = CoreM (mx >>= (\x -> nop x))

instance MonadIO CoreM where
    liftIO = liftIOEnv . IOEnv.liftIO

-- | Lift an 'IO' operation into 'CoreM' while consuming its 'SimplCount'
liftIOWithCount :: IO (SimplCount, a) -> CoreM a
liftIOWithCount what = liftIO what >>= (\(count, x) -> addSimplCount count >> return x)

{-
************************************************************************
*                                                                      *
             Reader, writer and state accessors
*                                                                      *
************************************************************************
-}

getHscEnv :: CoreM HscEnv
getHscEnv = read cr_hsc_env

getRuleBase :: CoreM RuleBase
getRuleBase = read cr_rule_base

getExternalRuleBase :: CoreM RuleBase
getExternalRuleBase = eps_rule_base <$> get_eps

getVisibleOrphanMods :: CoreM ModuleSet
getVisibleOrphanMods = read cr_visible_orphan_mods

getPrintUnqualified :: CoreM PrintUnqualified
getPrintUnqualified = read cr_print_unqual

getSrcSpanM :: CoreM SrcSpan
getSrcSpanM = read cr_loc

addSimplCount :: SimplCount -> CoreM ()
addSimplCount count = write (CoreWriter { cw_simpl_count = count })

getUniqMask :: CoreM Char
getUniqMask = read cr_uniq_mask

-- Convenience accessors for useful fields of HscEnv

-- | Adjust the dyn flags passed to the arugment action
mapDynFlagsCoreM :: (DynFlags -> DynFlags) -> CoreM a -> CoreM a
mapDynFlagsCoreM f m = CoreM $ do
  !e <- getEnv
  let !e' = e { cr_hsc_env = hscUpdateFlags f $ cr_hsc_env e }
  liftIO $ runIOEnv e' $! unCoreM m

-- | Drop the single count of the argument action so it doesn't effect
-- the total.
dropSimplCount :: CoreM a -> CoreM a
dropSimplCount m = CoreM $ do
  (a, _) <- unCoreM m
  unCoreM $ pure a

instance HasDynFlags CoreM where
    getDynFlags = fmap hsc_dflags getHscEnv

instance HasLogger CoreM where
    getLogger = fmap hsc_logger getHscEnv

instance HasModule CoreM where
    getModule = read cr_module

getInteractiveContext :: CoreM InteractiveContext
getInteractiveContext = hsc_IC <$> getHscEnv

getPackageFamInstEnv :: CoreM PackageFamInstEnv
getPackageFamInstEnv = eps_fam_inst_env <$> get_eps

get_eps :: CoreM ExternalPackageState
get_eps = do
    hsc_env <- getHscEnv
    liftIO $ hscEPS hsc_env

{-
************************************************************************
*                                                                      *
             Dealing with annotations
*                                                                      *
************************************************************************
-}

-- | Get all annotations of a given type. This happens lazily, that is
-- no deserialization will take place until the [a] is actually demanded and
-- the [a] can also be empty (the UniqFM is not filtered).
--
-- This should be done once at the start of a Core-to-Core pass that uses
-- annotations.
--
-- See Note [Annotations]
getAnnotations :: Typeable a => ([Word8] -> a) -> ModGuts -> CoreM (ModuleEnv [a], NameEnv [a])
getAnnotations deserialize guts = do
     hsc_env <- getHscEnv
     liftIO $ getAnnotationsFromHscEnv hsc_env deserialize guts

-- | Get at most one annotation of a given type per annotatable item.
getFirstAnnotations :: Typeable a => ([Word8] -> a) -> ModGuts -> CoreM (ModuleEnv a, NameEnv a)
getFirstAnnotations deserialize guts = do
     hsc_env <- getHscEnv
     liftIO $ getFirstAnnotationsFromHscEnv hsc_env deserialize guts

{-
************************************************************************
*                                                                      *
                Direct screen output
*                                                                      *
************************************************************************
-}

msg :: MessageClass -> SDoc -> CoreM ()
msg msg_class doc = do
    logger <- getLogger
    loc    <- getSrcSpanM
    unqual <- getPrintUnqualified
    let sty = case msg_class of
                MCDiagnostic _ _ -> err_sty
                MCDump           -> dump_sty
                _                -> user_sty
        err_sty  = mkErrStyle unqual
        user_sty = mkUserStyle unqual AllTheWay
        dump_sty = mkDumpStyle unqual
    liftIO $ logMsg logger msg_class loc (withPprStyle sty doc)

-- | Output a String message to the screen
putMsgS :: String -> CoreM ()
putMsgS = putMsg . text

-- | Output a message to the screen
putMsg :: SDoc -> CoreM ()
putMsg = msg MCInfo

-- | Output an error to the screen. Does not cause the compiler to die.
errorMsgS :: String -> CoreM ()
errorMsgS = errorMsg . text

-- | Output an error to the screen. Does not cause the compiler to die.
errorMsg :: SDoc -> CoreM ()
errorMsg doc = msg errorDiagnostic doc

-- | Output a fatal error to the screen. Does not cause the compiler to die.
fatalErrorMsgS :: String -> CoreM ()
fatalErrorMsgS = fatalErrorMsg . text

-- | Output a fatal error to the screen. Does not cause the compiler to die.
fatalErrorMsg :: SDoc -> CoreM ()
fatalErrorMsg = msg MCFatal

-- | Output a string debugging message at verbosity level of @-v@ or higher
debugTraceMsgS :: String -> CoreM ()
debugTraceMsgS = debugTraceMsg . text

-- | Outputs a debugging message at verbosity level of @-v@ or higher
debugTraceMsg :: SDoc -> CoreM ()
debugTraceMsg = msg MCDump

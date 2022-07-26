{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[SimplCore]{Driver for simplifying @Core@ programs}
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}

module GHC.Core.Opt.Pipeline ( core2core, simplifyExpr ) where

import GHC.Prelude

import GHC.Driver.Session
import GHC.Driver.Plugins ( withPlugins, installCoreToDos )
import GHC.Driver.Env
import GHC.Driver.Config.Core.Lint ( initLintAnnotationsConfig )
import GHC.Driver.Config.Core.EndPass ( initEndPassConfig )
import GHC.Driver.Config.Core.Opt.CallerCC ( initCallerCCOpts )
import GHC.Driver.Config.Core.Opt.SpecConstr ( initSpecConstrOpts )
import GHC.Driver.Config.Core.Opt.Specialise ( initSpecialiseOpts )
import GHC.Driver.Config.Core.Opt.LiberateCase ( initLiberateCaseOpts )
import GHC.Driver.Config.Core.Opt.Simplify ( initSimplifyOpts, initSimplMode, initGentleSimplMode )
import GHC.Driver.Config.Core.Opt.WorkWrap ( initWorkWrapOpts )
import GHC.Driver.Config.Core.Rules ( initRuleOpts )
import GHC.Platform.Ways  ( hasWay, Way(WayProf) )

import GHC.Core
import GHC.Core.EndPass  ( endPassIO )
import GHC.Core.Opt.CSE  ( cseProgram )
import GHC.Core.Rules   ( extendRuleBaseList, extendRuleEnv, mkRuleBase, ruleCheckProgram, getRules )
import GHC.Core.Ppr     ( pprCoreBindings )
import GHC.Core.Utils   ( dumpIdInfoOfProgram )
import GHC.Core.Lint    ( lintAnnots )
import GHC.Core.Lint.Interactive ( interactiveInScope )
import GHC.Core.Opt.Pipeline.Types ( CoreToDo(..) )
import GHC.Core.Opt.Simplify ( simplifyExpr, simplifyPgm )
import GHC.Core.Opt.Simplify.Monad
import GHC.Core.Opt.Stats        ( SimplCountM, addCounts, runSimplCountM )
import GHC.Core.Opt.Utils        ( FloatOutSwitches(..)
                                 , getFirstAnnotationsFromHscEnv )
import GHC.Core.Opt.FloatIn      ( floatInwards )
import GHC.Core.Opt.FloatOut     ( floatOutwards )
import GHC.Core.Opt.LiberateCase ( liberateCase )
import GHC.Core.Opt.StaticArgs   ( doStaticArgs )
import GHC.Core.Opt.Specialise   ( specProgram )
import GHC.Core.Opt.SpecConstr   ( specConstrProgram )
import GHC.Core.Opt.DmdAnal
import GHC.Core.Opt.CprAnal      ( cprAnalProgram )
import GHC.Core.Opt.CallArity    ( callArityAnalProgram )
import GHC.Core.Opt.Exitify      ( exitifyProgram )
import GHC.Core.Opt.WorkWrap     ( wwTopBinds )
import GHC.Core.Opt.CallerCC     ( addCallerCostCentres )
import GHC.Core.LateCC           (addLateCostCentresMG)
import GHC.Core.Seq (seqBinds)
import GHC.Core.FamInstEnv

import GHC.Plugins.Monad

import GHC.Serialized   ( deserializeWithData )

import GHC.Utils.Error  ( withTiming )
import GHC.Utils.Logger as Logger
import GHC.Utils.Outputable

import GHC.Unit.External ( ExternalPackageState(..) )
import GHC.Unit.Module.Env
import GHC.Unit.Module.ModGuts
import GHC.Unit.Module.Deps

import GHC.Types.Id.Info
import GHC.Types.Basic
import GHC.Types.Demand ( zapDmdEnvSig )
import GHC.Types.SrcLoc ( SrcSpan )
import GHC.Types.Unique.Supply ( mkSplitUniqSupply )
import GHC.Types.Name.Ppr
import GHC.Types.Var ( Var )

import Control.Monad
import qualified GHC.LanguageExtensions as LangExt
import GHC.Unit.Module

{-
************************************************************************
*                                                                      *
\subsection{The driver for the simplifier}
*                                                                      *
************************************************************************
-}

core2core :: HscEnv -> ModGuts -> IO ModGuts
core2core hsc_env guts@(ModGuts { mg_module  = mod
                                , mg_loc     = loc
                                , mg_deps    = deps
                                , mg_rdr_env = rdr_env })
  = do { let builtin_passes = getCoreToDo dflags extra_vars
             orph_mods = mkModuleSet (mod : dep_orphs deps)

       ; (guts2, stats) <- runSimplCountM dump_simpl_stats $ do
           (all_passes, sc) <- liftIO $ runPlugin hsc_env guts $
             withPlugins (hsc_plugins hsc_env) installCoreToDos builtin_passes
           addCounts sc
           runCorePasses logger hsc_env hpt_rule_base loc
                         print_unqual orph_mods all_passes guts

       ; Logger.putDumpFileMaybe logger Opt_D_dump_simpl_stats
             "Grand total simplifier statistics"
             FormatText
             (pprSimplCount stats)

       ; return guts2 }
  where
    dflags           = hsc_dflags hsc_env
    logger           = hsc_logger hsc_env
    dump_simpl_stats = logHasDumpFlag logger Opt_D_dump_simpl_stats
    extra_vars       = interactiveInScope (hsc_IC hsc_env)
    home_pkg_rules   = hptRules hsc_env (moduleUnitId mod) (GWIB { gwib_mod = moduleName mod
                                                                 , gwib_isBoot = NotBoot })
    hpt_rule_base  = mkRuleBase home_pkg_rules
    print_unqual   = mkPrintUnqualified (hsc_unit_env hsc_env) rdr_env
    -- mod: get the module out of the current HscEnv so we can retrieve it from the monad.
    -- This is very convienent for the users of the monad (e.g. plugins do not have to
    -- consume the ModGuts to find the module) but somewhat ugly because mg_module may
    -- _theoretically_ be changed during the Core pipeline (it's part of ModGuts), which
    -- would mean our cached value would go out of date.

{-
************************************************************************
*                                                                      *
           Generating the main optimisation pipeline
*                                                                      *
************************************************************************
-}

getCoreToDo :: DynFlags -> [Var] -> [CoreToDo]
getCoreToDo dflags extra_vars
  = flatten_todos core_todo
  where
    phases        = simplPhases        dflags
    max_iter      = maxSimplIterations dflags
    rule_check    = ruleCheck          dflags
    const_fold    = gopt Opt_CoreConstantFolding          dflags
    call_arity    = gopt Opt_CallArity                    dflags
    exitification = gopt Opt_Exitification                dflags
    strictness    = gopt Opt_Strictness                   dflags
    full_laziness = gopt Opt_FullLaziness                 dflags
    do_specialise = gopt Opt_Specialise                   dflags
    do_float_in   = gopt Opt_FloatIn                      dflags
    cse           = gopt Opt_CSE                          dflags
    spec_constr   = gopt Opt_SpecConstr                   dflags
    liberate_case = gopt Opt_LiberateCase                 dflags
    late_dmd_anal = gopt Opt_LateDmdAnal                  dflags
    late_specialise = gopt Opt_LateSpecialise             dflags
    static_args   = gopt Opt_StaticArgumentTransformation dflags
    rules_on      = gopt Opt_EnableRewriteRules           dflags
    ww_on         = gopt Opt_WorkerWrapper                dflags
    static_ptrs   = xopt LangExt.StaticPointers           dflags
    profiling     = ways dflags `hasWay` WayProf

    do_presimplify = do_specialise -- TODO: any other optimizations benefit from pre-simplification?
    do_simpl3      = const_fold || rules_on -- TODO: any other optimizations benefit from three-phase simplification?

    maybe_rule_check phase = runMaybe rule_check (CoreDoRuleCheck phase)

    maybe_strictness_before (Phase phase)
      | phase `elem` strictnessBefore dflags = CoreDoDemand
    maybe_strictness_before _
      = CoreDoNothing

    simpl_phase phase name iter
      = CoreDoPasses
      $   [ maybe_strictness_before phase
          , CoreDoSimplify $ initSimplifyOpts dflags extra_vars iter
                             (initSimplMode dflags phase name)
          , maybe_rule_check phase ]

    -- Run GHC's internal simplification phase, after all rules have run.
    -- See Note [Compiler phases] in GHC.Types.Basic
    simplify name = simpl_phase FinalPhase name max_iter

    -- initial simplify: mk specialiser happy: minimum effort please
    -- See Note [Inline in InitialPhase]
    -- See Note [RULEs enabled in InitialPhase]
    simpl_gently = CoreDoSimplify $ initSimplifyOpts dflags extra_vars max_iter
                                    (initGentleSimplMode dflags)

    dmd_cpr_ww = if ww_on then [CoreDoDemand,CoreDoCpr,CoreDoWorkerWrapper]
                          else [CoreDoDemand,CoreDoCpr]


    demand_analyser = (CoreDoPasses (
                           dmd_cpr_ww ++
                           [simplify "post-worker-wrapper"]
                           ))

    -- Static forms are moved to the top level with the FloatOut pass.
    -- See Note [Grand plan for static forms] in GHC.Iface.Tidy.StaticPtrTable.
    static_ptrs_float_outwards =
      runWhen static_ptrs $ CoreDoPasses
        [ simpl_gently -- Float Out can't handle type lets (sometimes created
                       -- by simpleOptPgm via mkParallelBindings)
        , CoreDoFloatOutwards FloatOutSwitches
          { floatOutLambdas   = Just 0
          , floatOutConstants = True
          , floatOutOverSatApps = False
          , floatToTopLevelOnly = True
          }
        ]

    add_caller_ccs =
        runWhen (profiling && not (null $ callerCcFilters dflags)) CoreAddCallerCcs

    add_late_ccs =
        runWhen (profiling && gopt Opt_ProfLateInlineCcs dflags) $ CoreAddLateCcs

    core_todo =
     [
    -- We want to do the static argument transform before full laziness as it
    -- may expose extra opportunities to float things outwards. However, to fix
    -- up the output of the transformation we need at do at least one simplify
    -- after this before anything else
        runWhen static_args (CoreDoPasses [ simpl_gently, CoreDoStaticArgs ]),

        -- initial simplify: mk specialiser happy: minimum effort please
        runWhen do_presimplify simpl_gently,

        -- Specialisation is best done before full laziness
        -- so that overloaded functions have all their dictionary lambdas manifest
        runWhen do_specialise CoreDoSpecialising,

        if full_laziness then
           CoreDoFloatOutwards FloatOutSwitches {
                                 floatOutLambdas   = Just 0,
                                 floatOutConstants = True,
                                 floatOutOverSatApps = False,
                                 floatToTopLevelOnly = False }
                -- Was: gentleFloatOutSwitches
                --
                -- I have no idea why, but not floating constants to
                -- top level is very bad in some cases.
                --
                -- Notably: p_ident in spectral/rewrite
                --          Changing from "gentle" to "constantsOnly"
                --          improved rewrite's allocation by 19%, and
                --          made 0.0% difference to any other nofib
                --          benchmark
                --
                -- Not doing floatOutOverSatApps yet, we'll do
                -- that later on when we've had a chance to get more
                -- accurate arity information.  In fact it makes no
                -- difference at all to performance if we do it here,
                -- but maybe we save some unnecessary to-and-fro in
                -- the simplifier.
        else
           -- Even with full laziness turned off, we still need to float static
           -- forms to the top level. See Note [Grand plan for static forms] in
           -- GHC.Iface.Tidy.StaticPtrTable.
           static_ptrs_float_outwards,

        -- Run the simplier phases 2,1,0 to allow rewrite rules to fire
        runWhen do_simpl3
            (CoreDoPasses $ [ simpl_phase (Phase phase) "main" max_iter
                            | phase <- [phases, phases-1 .. 1] ] ++
                            [ simpl_phase (Phase 0) "main" (max max_iter 3) ]),
                -- Phase 0: allow all Ids to be inlined now
                -- This gets foldr inlined before strictness analysis

                -- At least 3 iterations because otherwise we land up with
                -- huge dead expressions because of an infelicity in the
                -- simplifier.
                --      let k = BIG in foldr k z xs
                -- ==>  let k = BIG in letrec go = \xs -> ...(k x).... in go xs
                -- ==>  let k = BIG in letrec go = \xs -> ...(BIG x).... in go xs
                -- Don't stop now!

        runWhen do_float_in CoreDoFloatInwards,
            -- Run float-inwards immediately before the strictness analyser
            -- Doing so pushes bindings nearer their use site and hence makes
            -- them more likely to be strict. These bindings might only show
            -- up after the inlining from simplification.  Example in fulsom,
            -- Csg.calc, where an arg of timesDouble thereby becomes strict.

        runWhen call_arity $ CoreDoPasses
            [ CoreDoCallArity
            , simplify "post-call-arity"
            ],

        -- Strictness analysis
        runWhen strictness demand_analyser,

        runWhen exitification CoreDoExitify,
            -- See Note [Placement of the exitification pass]

        runWhen full_laziness $
           CoreDoFloatOutwards FloatOutSwitches {
                                 floatOutLambdas     = floatLamArgs dflags,
                                 floatOutConstants   = True,
                                 floatOutOverSatApps = True,
                                 floatToTopLevelOnly = False },
                -- nofib/spectral/hartel/wang doubles in speed if you
                -- do full laziness late in the day.  It only happens
                -- after fusion and other stuff, so the early pass doesn't
                -- catch it.  For the record, the redex is
                --        f_el22 (f_el21 r_midblock)


        runWhen cse CoreCSE,
                -- We want CSE to follow the final full-laziness pass, because it may
                -- succeed in commoning up things floated out by full laziness.
                -- CSE used to rely on the no-shadowing invariant, but it doesn't any more

        runWhen do_float_in CoreDoFloatInwards,

        simplify "final",  -- Final tidy-up

        maybe_rule_check FinalPhase,

        --------  After this we have -O2 passes -----------------
        -- None of them run with -O

                -- Case-liberation for -O2.  This should be after
                -- strictness analysis and the simplification which follows it.
        runWhen liberate_case $ CoreDoPasses
           [ CoreLiberateCase, simplify "post-liberate-case" ],
           -- Run the simplifier after LiberateCase to vastly
           -- reduce the possibility of shadowing
           -- Reason: see Note [Shadowing] in GHC.Core.Opt.SpecConstr

        runWhen spec_constr $ CoreDoPasses
           [ CoreDoSpecConstr, simplify "post-spec-constr"],
           -- See Note [Simplify after SpecConstr]

        maybe_rule_check FinalPhase,

        runWhen late_specialise $ CoreDoPasses
           [ CoreDoSpecialising, simplify "post-late-spec"],

        -- LiberateCase can yield new CSE opportunities because it peels
        -- off one layer of a recursive function (concretely, I saw this
        -- in wheel-sieve1), and I'm guessing that SpecConstr can too
        -- And CSE is a very cheap pass. So it seems worth doing here.
        runWhen ((liberate_case || spec_constr) && cse) $ CoreDoPasses
           [ CoreCSE, simplify "post-final-cse" ],

        ---------  End of -O2 passes --------------

        runWhen late_dmd_anal $ CoreDoPasses (
            dmd_cpr_ww ++ [simplify "post-late-ww"]
          ),

        -- Final run of the demand_analyser, ensures that one-shot thunks are
        -- really really one-shot thunks. Only needed if the demand analyser
        -- has run at all. See Note [Final Demand Analyser run] in GHC.Core.Opt.DmdAnal
        -- It is EXTREMELY IMPORTANT to run this pass, otherwise execution
        -- can become /exponentially/ more expensive. See #11731, #12996.
        runWhen (strictness || late_dmd_anal) CoreDoDemand,

        maybe_rule_check FinalPhase,

        add_caller_ccs,
        add_late_ccs
     ]

    -- Remove 'CoreDoNothing' and flatten 'CoreDoPasses' for clarity.
    flatten_todos [] = []
    flatten_todos (CoreDoNothing : rest) = flatten_todos rest
    flatten_todos (CoreDoPasses passes : rest) =
      flatten_todos passes ++ flatten_todos rest
    flatten_todos (todo : rest) = todo : flatten_todos rest

-- The core-to-core pass ordering is derived from the DynFlags:
runWhen :: Bool -> CoreToDo -> CoreToDo
runWhen True  do_this = do_this
runWhen False _       = CoreDoNothing

runMaybe :: Maybe a -> (a -> CoreToDo) -> CoreToDo
runMaybe (Just x) f = f x
runMaybe Nothing  _ = CoreDoNothing

{- Note [Inline in InitialPhase]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In GHC 8 and earlier we did not inline anything in the InitialPhase. But that is
confusing for users because when they say INLINE they expect the function to inline
right away.

So now we do inlining immediately, even in the InitialPhase, assuming that the
Id's Activation allows it.

This is a surprisingly big deal. Compiler performance improved a lot
when I made this change:

   perf/compiler/T5837.run            T5837 [stat too good] (normal)
   perf/compiler/parsing001.run       parsing001 [stat too good] (normal)
   perf/compiler/T12234.run           T12234 [stat too good] (optasm)
   perf/compiler/T9020.run            T9020 [stat too good] (optasm)
   perf/compiler/T3064.run            T3064 [stat too good] (normal)
   perf/compiler/T9961.run            T9961 [stat too good] (normal)
   perf/compiler/T13056.run           T13056 [stat too good] (optasm)
   perf/compiler/T9872d.run           T9872d [stat too good] (normal)
   perf/compiler/T783.run             T783 [stat too good] (normal)
   perf/compiler/T12227.run           T12227 [stat too good] (normal)
   perf/should_run/lazy-bs-alloc.run  lazy-bs-alloc [stat too good] (normal)
   perf/compiler/T1969.run            T1969 [stat too good] (normal)
   perf/compiler/T9872a.run           T9872a [stat too good] (normal)
   perf/compiler/T9872c.run           T9872c [stat too good] (normal)
   perf/compiler/T9872b.run           T9872b [stat too good] (normal)
   perf/compiler/T9872d.run           T9872d [stat too good] (normal)

Note [RULEs enabled in InitialPhase]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
RULES are enabled when doing "gentle" simplification in InitialPhase,
or with -O0.  Two reasons:

  * We really want the class-op cancellation to happen:
        op (df d1 d2) --> $cop3 d1 d2
    because this breaks the mutual recursion between 'op' and 'df'

  * I wanted the RULE
        lift String ===> ...
    to work in Template Haskell when simplifying
    splices, so we get simpler code for literal strings

But watch out: list fusion can prevent floating.  So use phase control
to switch off those rules until after floating.

Note [Simplify after SpecConstr]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to run the simplifier after SpecConstr, and before late-Specialise,
for two reasons, both shown up in test perf/compiler/T16473,
with -O2 -flate-specialise

1.  I found that running late-Specialise after SpecConstr, with no
    simplification in between meant that the carefullly constructed
    SpecConstr rule never got to fire.  (It was something like
          lvl = f a   -- Arity 1
          ....g lvl....
    SpecConstr specialised g for argument lvl; but Specialise then
    specialised lvl = f a to lvl = $sf, and inlined. Or something like
    that.)

2.  Specialise relies on unfoldings being available for top-level dictionary
    bindings; but SpecConstr kills them all!  The Simplifer restores them.

This extra run of the simplifier has a cost, but this is only with -O2.


************************************************************************
*                                                                      *
                  The CoreToDo interpreter
*                                                                      *
************************************************************************
-}

runCorePasses :: Logger
              -> HscEnv
              -> RuleBase
              -> SrcSpan
              -> PrintUnqualified
              -> ModuleSet
              -> [CoreToDo]
              -> ModGuts
              -> SimplCountM ModGuts
runCorePasses logger hsc_env rule_base loc print_unqual vis_orphs
              passes guts
  = foldM do_pass guts passes
  where
    do_pass :: ModGuts -> CoreToDo -> SimplCountM ModGuts
    do_pass res CoreDoNothing = return res
    do_pass guts (CoreDoPasses ps) = runCorePasses logger hsc_env rule_base loc print_unqual vis_orphs ps guts
    do_pass guts pass = do
      let extra_vars = interactiveInScope $ hsc_IC hsc_env
      let end_pass_cfg = initEndPassConfig dflags extra_vars print_unqual pass
      let lint_anno_cfg = initLintAnnotationsConfig dflags loc print_unqual pass
      let doCorePassWithDebug debug_lvl nguts = do
            let dflags' = (hsc_dflags hsc_env) { debugLevel = debug_lvl }
                hsc_env' = hsc_env { hsc_dflags = dflags' }
            doCorePass
              logger hsc_env' this_mod rule_base loc print_unqual vis_orphs
              pass nguts

      withTiming logger (ppr pass <+> brackets (ppr this_mod)) (const ()) $ do
        guts' <- lintAnnots logger lint_anno_cfg doCorePassWithDebug guts
        liftIO $ endPassIO logger end_pass_cfg (mg_binds guts') (mg_rules guts')
        return guts'

    dflags = hsc_dflags hsc_env
    this_mod = mg_module guts

doCorePass :: Logger
           -> HscEnv
           -> Module
           -> RuleBase
           -> SrcSpan
           -> PrintUnqualified
           -> ModuleSet
           -> CoreToDo
           -> ModGuts
           -> SimplCountM ModGuts
doCorePass logger hsc_env this_mod rule_base loc print_unqual vis_orphs pass guts = do
  eps <- liftIO $ hscEPS hsc_env
  (_, annos) <- liftIO $ getFirstAnnotationsFromHscEnv hsc_env deserializeWithData guts
  us <- liftIO $ mkSplitUniqSupply simplMask

  -- let this_mod = mg_module guts
  let dflags = hsc_dflags hsc_env
  let external_rule_base = eps_rule_base eps
  let p_fam_env = eps_fam_inst_env eps
  let platform = targetPlatform dflags
  let fam_envs = (p_fam_env, mg_fam_inst_env guts)
  let prof_count_entries = gopt Opt_ProfCountEntries dflags
  let !read_ruleenv = readRuleEnv hsc_env guts

  case pass of
    CoreDoSimplify opts       -> {-# SCC "Simplify" #-} do
                                 (guts', sc) <- liftIO $ simplifyPgm logger read_ruleenv (hsc_unit_env hsc_env) opts guts
                                 addCounts sc
                                 return guts'

    CoreCSE                   -> {-# SCC "CommonSubExpr" #-}
                                 updateBinds cseProgram

    CoreLiberateCase          -> {-# SCC "LiberateCase" #-} do
                                 let opts = initLiberateCaseOpts dflags
                                 updateBinds (liberateCase opts)

    CoreDoFloatInwards        -> {-# SCC "FloatInwards" #-}
                                 updateBinds (floatInwards platform)

    CoreDoFloatOutwards f     -> {-# SCC "FloatOutwards" #-}
                                 updateBindsM (floatOutwards logger f us)

    CoreDoStaticArgs          -> {-# SCC "StaticArgs" #-}
                                 updateBinds (doStaticArgs us)

    CoreDoCallArity           -> {-# SCC "CallArity" #-}
                                 updateBinds callArityAnalProgram

    CoreDoExitify             -> {-# SCC "Exitify" #-}
                                 updateBinds exitifyProgram

    CoreDoDemand              -> {-# SCC "DmdAnal" #-}
                                 updateBindsM (dmdAnal logger dflags fam_envs (mg_rules guts))

    CoreDoCpr                 -> {-# SCC "CprAnal" #-}
                                 updateBindsM (cprAnalProgram logger fam_envs)

    CoreDoWorkerWrapper       -> {-# SCC "WorkWrap" #-} do
                                 let opts = initWorkWrapOpts (mg_module guts) dflags fam_envs
                                 updateBinds (wwTopBinds opts us)

    CoreDoSpecialising        -> {-# SCC "Specialise" #-} do
                                 let opts = initSpecialiseOpts dflags loc simplMask print_unqual
                                 liftIO $ specProgram logger opts vis_orphs external_rule_base rule_base guts

    CoreDoSpecConstr          -> {-# SCC "SpecConstr" #-} do
                                 let opts = initSpecConstrOpts dflags this_mod
                                 return (specConstrProgram annos us opts guts)

    CoreAddCallerCcs          -> {-# SCC "AddCallerCcs" #-} do
                                 let opts = initCallerCCOpts dflags
                                 return (addCallerCostCentres opts guts)

    CoreAddLateCcs            -> {-# SCC "AddLateCcs" #-}
                                 return (addLateCostCentresMG prof_count_entries guts)

    CoreDoPrintCore           -> {-# SCC "PrintCore" #-} do
                                 liftIO $ printCore logger (mg_binds guts)
                                 return guts

    CoreDoRuleCheck phase pat -> {-# SCC "RuleCheck" #-}
                                 liftIO $ ruleCheckPass logger dflags rule_base
                                                        vis_orphs phase pat guts

    CoreDoNothing             -> return guts

    CoreDoPasses passes       -> runCorePasses logger hsc_env rule_base loc
                                               print_unqual vis_orphs passes guts

    CoreDoPluginPass _ p      -> {-# SCC "Plugin" #-} do
                                 (guts', sc) <- liftIO $ runPlugin hsc_env guts $ p guts
                                 addCounts sc
                                 return guts'
  where
    updateBinds f = return $ guts { mg_binds = f (mg_binds guts) }
    updateBindsM f = do
      b' <- liftIO $ f (mg_binds guts)
      return $ guts { mg_binds = b' }

readRuleEnv :: HscEnv -> ModGuts -> IO RuleEnv
readRuleEnv hsc_env guts = extendRuleEnv base_ruleenv <$> read_eps_rules
  where
    this_mod = mg_module guts
    gwib = GWIB { gwib_mod = moduleName this_mod, gwib_isBoot = NotBoot }
    hpt_rule_base = mkRuleBase (hptRules hsc_env (moduleUnitId this_mod) gwib)
    -- Forcing this value to avoid unnessecary allocations.
    -- Not doing so results in +25.6% allocations of LargeRecord.
    !rule_base = extendRuleBaseList hpt_rule_base (mg_rules guts)
    vis_orphs = this_mod : dep_orphs (mg_deps guts)
    base_ruleenv = mkRuleEnv rule_base vis_orphs
    read_eps_rules = eps_rule_base <$> hscEPS hsc_env

runPlugin :: HscEnv -> ModGuts -> CoreM a -> IO (a, SimplCount)
runPlugin hsc_env guts m
  = runCoreM hsc_env hpt_rule_base simplMask mod orph_mods print_unqual loc m
  where
    mod = mg_module guts
    loc = mg_loc guts
    orph_mods = mkModuleSet (mod : dep_orphs (mg_deps guts))
    gwib = GWIB { gwib_mod = moduleName mod, gwib_isBoot = NotBoot }
    hpt_rule_base = mkRuleBase (hptRules hsc_env (moduleUnitId mod) gwib)
    print_unqual = mkPrintUnqualified (hsc_unit_env hsc_env) (mg_rdr_env guts)

-- TODO: This is the same as in GHC.Core.Opt.Simplify.Monad
-- TODO: Link to note
simplMask :: Char
simplMask = 's'

{-
************************************************************************
*                                                                      *
\subsection{Core pass combinators}
*                                                                      *
************************************************************************
-}

printCore :: Logger -> CoreProgram -> IO ()
printCore logger binds
    = Logger.logDumpMsg logger "Print Core" (pprCoreBindings binds)

ruleCheckPass :: Logger
              -> DynFlags
              -> RuleBase
              -> ModuleSet
              -> CompilerPhase
              -> String
              -> ModGuts
              -> IO ModGuts
ruleCheckPass logger dflags rb vis_orphs current_phase pat guts =
    withTiming logger (text "RuleCheck"<+>brackets (ppr $ mg_module guts))
                (const ()) $ do
        let rule_fn fn = getRules (RuleEnv [rb] vis_orphs) fn
                          ++ (mg_rules guts)
        let ropts = initRuleOpts dflags
        logDumpMsg logger "Rule check" $
            ruleCheckProgram ropts current_phase pat rule_fn (mg_binds guts)
        return guts

dmdAnal :: Logger -> DynFlags -> FamInstEnvs -> [CoreRule] -> CoreProgram -> IO CoreProgram
dmdAnal logger dflags fam_envs rules binds = do
  let !opts = DmdAnalOpts
               { dmd_strict_dicts    = gopt Opt_DictsStrict dflags
               , dmd_unbox_width     = dmdUnboxWidth dflags
               , dmd_max_worker_args = maxWorkerArgs dflags
               }
      binds_plus_dmds = dmdAnalProgram opts fam_envs rules binds
  Logger.putDumpFileMaybe logger Opt_D_dump_str_signatures "Strictness signatures" FormatText $
    dumpIdInfoOfProgram (hasPprDebug dflags) (ppr . zapDmdEnvSig . dmdSigInfo) binds_plus_dmds
  -- See Note [Stamp out space leaks in demand analysis] in GHC.Core.Opt.DmdAnal
  seqBinds binds_plus_dmds `seq` return binds_plus_dmds

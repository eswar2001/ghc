{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[SimplCore]{Driver for simplifying @Core@ programs}
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}

module GHC.Driver.Core.Opt ( runCorePasses ) where

import GHC.Prelude

import GHC.Driver.Session
import GHC.Driver.Env
import GHC.Driver.Core.Rules ( readRuleEnv )
import GHC.Driver.Config.Core.Lint ( initLintAnnotationsConfig )
import GHC.Driver.Config.Core.EndPass ( initEndPassConfig )
import GHC.Driver.Config.Core.Opt.Specialise ( initSpecialiseOpts )
import GHC.Driver.Config.Core.Opt.WorkWrap ( initWorkWrapOpts )
import GHC.Driver.Config.Core.Rules ( initRuleOpts )

import GHC.Core
import GHC.Core.EndPass  ( endPassIO )
import GHC.Core.Opt.CSE  ( cseProgram )
import GHC.Core.Rules   ( ruleCheckProgram, getRules )
import GHC.Core.Ppr     ( pprCoreBindings )
import GHC.Core.Utils   ( dumpIdInfoOfProgram )
import GHC.Core.Lint    ( lintAnnots )
import GHC.Core.Lint.Interactive ( interactiveInScope )
import GHC.Core.Opt.Config ( CoreToDo(..) )
import GHC.Core.Opt.Simplify ( simplifyPgm )
import GHC.Core.Opt.Simplify.Monad
import GHC.Core.Opt.Stats        ( SimplCountM, addCounts )
import GHC.Core.Opt.Utils        ( getFirstAnnotationsFromHscEnv )
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
import GHC.Core.LateCC           (addLateCostCentres)
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

import GHC.Types.Id.Info
import GHC.Types.Basic
import GHC.Types.Demand ( zapDmdEnvSig )
import GHC.Types.SrcLoc ( SrcSpan )
import GHC.Types.Unique.Supply ( mkSplitUniqSupply )

import Control.Monad
import GHC.Unit.Module

{-
************************************************************************
*                                                                      *
                  The CoreToDo interpreter
*                                                                      *
************************************************************************
-}

-- | Run a core pipeline, as specified with a list of 'CoreToDo'.
--
-- In typical ussage, the "plannning" of what passes to run (i.e.
-- creation of the '[CoreToDo]') happens in
-- 'GHC.Driver.Config.Core.Opt'. Then this function "executes" that
-- plan.
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

-- | Run a single core pass, as specified with a single 'CoreToDo'.
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

  let dflags = hsc_dflags hsc_env
  let external_rule_base = eps_rule_base eps
  let p_fam_env = eps_fam_inst_env eps
  let fam_envs = (p_fam_env, mg_fam_inst_env guts)
  let !read_ruleenv = readRuleEnv hsc_env guts
  let unit_env = hsc_unit_env hsc_env

  case pass of
    CoreDoSimplify opts       -> {-# SCC "Simplify" #-} do
                                 (guts', sc) <- liftIO $ simplifyPgm logger read_ruleenv unit_env opts guts
                                 addCounts sc
                                 return guts'

    CoreCSE                   -> {-# SCC "CommonSubExpr" #-}
                                 updateBinds cseProgram

    CoreLiberateCase opts     -> {-# SCC "LiberateCase" #-} do
                                 updateBinds (liberateCase opts)

    CoreDoFloatInwards opts   -> {-# SCC "FloatInwards" #-}
                                 updateBinds (floatInwards opts)

    CoreDoFloatOutwards opts  -> {-# SCC "FloatOutwards" #-}
                                 updateBindsM (floatOutwards logger opts us)

    CoreDoStaticArgs          -> {-# SCC "StaticArgs" #-}
                                 updateBinds (doStaticArgs us)

    CoreDoCallArity           -> {-# SCC "CallArity" #-}
                                 updateBinds callArityAnalProgram

    CoreDoExitify             -> {-# SCC "Exitify" #-}
                                 updateBinds exitifyProgram

    CoreDoDemand              -> {-# SCC "DmdAnal" #-}
                                 updateBindsM (dmdAnal logger dflags fam_envs (mg_rules guts))  -- TODO: dmdAnal takes DynFlags

    CoreDoCpr                 -> {-# SCC "CprAnal" #-}
                                 updateBindsM (cprAnalProgram logger fam_envs)

    CoreDoWorkerWrapper       -> {-# SCC "WorkWrap" #-} do
                                 let opts = initWorkWrapOpts (mg_module guts) dflags fam_envs  -- TODO: How to get rid of the module part ?
                                 updateBinds (wwTopBinds opts us)

    CoreDoSpecialising        -> {-# SCC "Specialise" #-} do
                                 let opts = initSpecialiseOpts dflags simplMask print_unqual
                                 liftIO $ specProgram logger opts loc vis_orphs external_rule_base rule_base guts

    CoreDoSpecConstr opts     -> {-# SCC "SpecConstr" #-}
                                 return (specConstrProgram annos us opts this_mod guts)

    CoreAddCallerCcs opts     -> {-# SCC "AddCallerCcs" #-}
                                 return (addCallerCostCentres opts guts)

    CoreAddLateCcs opts       -> {-# SCC "AddLateCcs" #-}
                                 return (addLateCostCentres opts guts)

    CoreDoPrintCore           -> {-# SCC "PrintCore" #-} do
                                 liftIO $ printCore logger (mg_binds guts)
                                 return guts

    CoreDoRuleCheck phase pat -> {-# SCC "RuleCheck" #-}
                                 liftIO $ ruleCheckPass logger (initRuleOpts dflags) rule_base  -- TODO: Own config record
                                                        vis_orphs phase pat guts

    CoreDoNothing             -> return guts

    CoreDoPasses passes       -> runCorePasses logger hsc_env rule_base loc
                                               print_unqual vis_orphs passes guts

    CoreDoPluginPass _ p      -> {-# SCC "Plugin" #-} do
                                 liftCoreMToSimplCountM hsc_env guts $ p guts
  where
    updateBinds f = return $ guts { mg_binds = f (mg_binds guts) }

    updateBindsM f = do
      b' <- liftIO $ f (mg_binds guts)
      return $ guts { mg_binds = b' }

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
              -> RuleOpts
              -> RuleBase
              -> ModuleSet
              -> CompilerPhase
              -> String
              -> ModGuts
              -> IO ModGuts
ruleCheckPass logger ropts rb vis_orphs current_phase pat guts =
    withTiming logger (text "RuleCheck"<+>brackets (ppr $ mg_module guts))
                (const ()) $ do
        let rule_fn fn = getRules (RuleEnv [rb] vis_orphs) fn
                          ++ (mg_rules guts)
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

{-# LANGUAGE LambdaCase #-}
module GHC.Driver.Env
   ( Hsc(..)
   , HscEnv (..)
   , hscUpdateFlags
   , hscSetFlags
   , hsc_home_unit
   , hsc_home_unit_maybe
   , hsc_units
   , hsc_HPT
   , hsc_HUE
   , hsc_HUG
   , hsc_all_home_unit_ids
   , hscUpdateLoggerFlags
   , hscUpdateHUG
   -- , hscUpdateHPT
   , hscSetActiveHomeUnit
   , hscSetActiveUnitId
   , hscActiveUnitId
   , runHsc
   , runHsc'
   , mkInteractiveHscEnv
   , runInteractiveHsc
   , hscEPS
   , hscInterp
   , prepareAnnotations
   , discardIC
   , lookupType
   , lookupIfaceByModule
   , mainModIs
   )
where

import GHC.Prelude

import GHC.Builtin.Names ( gHC_PRIM )

import GHC.Driver.DynFlags
import GHC.Driver.Errors ( printOrThrowDiagnostics )
import GHC.Driver.Errors.Types ( GhcMessage )
import GHC.Driver.Config.Logger (initLogFlags)
import GHC.Driver.Config.Diagnostic (initDiagOpts, initPrintConfig)
import GHC.Driver.Env.Types ( Hsc(..), HscEnv(..) )

import GHC.Runtime.Context
import GHC.Runtime.Interpreter.Types (Interp)

import GHC.Unit
import GHC.Unit.Module.ModGuts
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.ModDetails
import GHC.Unit.Home.ModInfo
import GHC.Unit.Home.PackageTable
import GHC.Unit.Home.Graph (HomeUnitGraph, HomeUnitEnv)
import qualified GHC.Unit.Home.Graph as HUG
import GHC.Unit.Env as UnitEnv
import GHC.Unit.External

import GHC.Core         ( CoreRule )
import GHC.Core.FamInstEnv
import GHC.Core.InstEnv

import GHC.Types.Annotations ( Annotation, AnnEnv, mkAnnEnv, plusAnnEnv )
import GHC.Types.CompleteMatch
import GHC.Types.Error ( emptyMessages, Messages )
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.TyThing

import GHC.Data.Maybe

import GHC.Utils.Exception as Ex
import GHC.Utils.Outputable
import GHC.Utils.Monad
import GHC.Utils.Panic
import GHC.Utils.Misc
import GHC.Utils.Logger

import Data.IORef
import qualified Data.Set as Set
import GHC.Unit.Module.Graph

runHsc :: HscEnv -> Hsc a -> IO a
runHsc hsc_env hsc = do
    (a, w) <- runHsc' hsc_env hsc
    let dflags = hsc_dflags hsc_env
    let !diag_opts = initDiagOpts dflags
        !print_config = initPrintConfig dflags
    printOrThrowDiagnostics (hsc_logger hsc_env) print_config diag_opts w
    return a

runHsc' :: HscEnv -> Hsc a -> IO (a, Messages GhcMessage)
runHsc' hsc_env (Hsc hsc) = hsc hsc_env emptyMessages

-- | Switches in the DynFlags and Plugins from the InteractiveContext
mkInteractiveHscEnv :: HscEnv -> HscEnv
mkInteractiveHscEnv hsc_env =
    let ic = hsc_IC hsc_env
    in hscSetFlags (ic_dflags ic) $
       hsc_env { hsc_plugins = ic_plugins ic }

-- | A variant of runHsc that switches in the DynFlags and Plugins from the
-- InteractiveContext before running the Hsc computation.
runInteractiveHsc :: HscEnv -> Hsc a -> IO a
runInteractiveHsc hsc_env = runHsc (mkInteractiveHscEnv hsc_env)

hsc_home_unit :: HscEnv -> HomeUnit
hsc_home_unit = unsafeGetHomeUnit . hsc_unit_env

hsc_home_unit_maybe :: HscEnv -> Maybe HomeUnit
hsc_home_unit_maybe = homeUnit . hsc_unit_env

hsc_units :: HasDebugCallStack => HscEnv -> UnitState
hsc_units = homeUnitState . hsc_unit_env

hsc_HPT :: HscEnv -> HomePackageTable
hsc_HPT = ue_hpt . hsc_unit_env

hsc_HUE :: HscEnv -> HomeUnitEnv
hsc_HUE = ue_currentHomeUnitEnv . hsc_unit_env

hsc_HUG :: HscEnv -> HomeUnitGraph
hsc_HUG = ue_home_unit_graph . hsc_unit_env

hsc_all_home_unit_ids :: HscEnv -> Set.Set UnitId
hsc_all_home_unit_ids = HUG.allUnits . hsc_HUG

hscInsertHPT :: ModuleName -> HomeModInfo -> HscEnv -> IO HscEnv
hscInsertHPT mn hmi hsc_env = do
  !res <- UnitEnv.insertHpt mn hmi (hsc_unit_env hsc_env)
  return $ hsc_env { hsc_unit_env = res }

hscUpdateHUG :: (HomeUnitGraph -> HomeUnitGraph) -> HscEnv -> HscEnv
hscUpdateHUG f hsc_env = hsc_env { hsc_unit_env = updateHug f (hsc_unit_env hsc_env) }

{-

Note [Target code interpreter]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Template Haskell and GHCi use an interpreter to execute code that is built for
the compiler target platform (= code host platform) on the compiler host
platform (= code build platform).

The internal interpreter can be used when both platforms are the same and when
the built code is compatible with the compiler itself (same way, etc.). This
interpreter is not always available: for instance stage1 compiler doesn't have
it because there might be an ABI mismatch between the code objects (built by
stage1 compiler) and the stage1 compiler itself (built by stage0 compiler).

In most cases, an external interpreter can be used instead: it runs in a
separate process and it communicates with the compiler via a two-way message
passing channel. The process is lazily spawned to avoid overhead when it is not
used.

The target code interpreter to use can be selected per session via the
`hsc_interp` field of `HscEnv`. There may be no interpreter available at all, in
which case Template Haskell and GHCi will fail to run. The interpreter to use is
configured via command-line flags (in `GHC.setSessionDynFlags`).


-}

-- Note [hsc_type_env_var hack]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- hsc_type_env_var is used to initialize tcg_type_env_var, and
-- eventually it is the mutable variable that is queried from
-- if_rec_types to get a TypeEnv.  So, clearly, it's something
-- related to knot-tying (see Note [Tying the knot]).
-- hsc_type_env_var is used in two places: initTcRn (where
-- it initializes tcg_type_env_var) and initIfaceCheck
-- (where it initializes if_rec_types).
--
-- But why do we need a way to feed a mutable variable in?  Why
-- can't we just initialize tcg_type_env_var when we start
-- typechecking?  The problem is we need to knot-tie the
-- EPS, and we may start adding things to the EPS before type
-- checking starts.
--
-- Here is a concrete example. Suppose we are running
-- "ghc -c A.hs", and we have this file system state:
--
--  A.hs-boot   A.hi-boot **up to date**
--  B.hs        B.hi      **up to date**
--  A.hs        A.hi      **stale**
--
-- The first thing we do is run checkOldIface on A.hi.
-- checkOldIface will call loadInterface on B.hi so it can
-- get its hands on the fingerprints, to find out if A.hi
-- needs recompilation.  But loadInterface also populates
-- the EPS!  And so if compilation turns out to be necessary,
-- as it is in this case, the thunks we put into the EPS for
-- B.hi need to have the correct if_rec_types mutable variable
-- to query.
--
-- If the mutable variable is only allocated WHEN we start
-- typechecking, then that's too late: we can't get the
-- information to the thunks.  So we need to pre-commit
-- to a type variable in 'hscIncrementalCompile' BEFORE we
-- check the old interface.
--
-- This is all a massive hack because arguably checkOldIface
-- should not populate the EPS. But that's a refactor for
-- another day.

-- | Retrieve the ExternalPackageState cache.
hscEPS :: HscEnv -> IO ExternalPackageState
hscEPS hsc_env = readIORef (euc_eps (ue_eps (hsc_unit_env hsc_env)))

-- | Deal with gathering annotations in from all possible places
--   and combining them into a single 'AnnEnv'
prepareAnnotations :: HscEnv -> Maybe ModGuts -> IO AnnEnv
prepareAnnotations hsc_env mb_guts = do
    eps <- hscEPS hsc_env
    let -- Extract annotations from the module being compiled if supplied one
        mb_this_module_anns = fmap (mkAnnEnv . mg_anns) mb_guts
        -- Extract dependencies of the module if we are supplied one,
        -- otherwise load annotations from all home package table
        -- entries regardless of dependency ordering.
        get_mod mg = (moduleUnitId (mg_module mg), GWIB (moduleName (mg_module mg)) NotBoot)
        home_pkg_anns  = (mkAnnEnv . hugAnns (hsc_unit_env hsc_env)) $ fmap get_mod mb_guts
        other_pkg_anns = eps_ann_env eps
        ann_env        = foldl1' plusAnnEnv $ catMaybes [mb_this_module_anns,
                                                         Just home_pkg_anns,
                                                         Just other_pkg_anns]
    return ann_env

-- | Find the 'TyThing' for the given 'Name' by using all the resources
-- at our disposal: the compiled modules in the 'HomePackageTable' and the
-- compiled modules in other packages that live in 'PackageTypeEnv'. Note
-- that this does NOT look up the 'TyThing' in the module being compiled: you
-- have to do that yourself, if desired
lookupType :: HscEnv -> Name -> IO (Maybe TyThing)
lookupType hsc_env name = do
   eps <- liftIO $ hscEPS hsc_env
   let pte = eps_PTE eps
   lookupTypeInPTE hsc_env pte name

lookupTypeInPTE :: HscEnv -> PackageTypeEnv -> Name -> IO (Maybe TyThing)
lookupTypeInPTE hsc_env pte name = ty
  where
    hpt = hsc_HUG hsc_env
    mod = assertPpr (isExternalName name) (ppr name) $
          if isHoleName name
            then mkHomeModule (hsc_home_unit hsc_env) (moduleName (nameModule name))
            else nameModule name

    ty = if isOneShot (ghcMode (hsc_dflags hsc_env))
            -- in one-shot, we don't use the HPT
            then return $! lookupNameEnv pte name
            else HUG.lookupHugByModule mod hpt >>= \case
             Just hm -> pure $! lookupNameEnv (md_types (hm_details hm)) name
             Nothing -> pure $! lookupNameEnv pte name

-- | Find the 'ModIface' for a 'Module', searching in both the loaded home
-- and external package module information
lookupIfaceByModule
        :: HomeUnitGraph
        -> PackageIfaceTable
        -> Module
        -> IO (Maybe ModIface)
lookupIfaceByModule hug pit mod
  = HUG.lookupHugByModule mod hug >>= pure . \case
       Just hm -> Just (hm_iface hm)
       Nothing -> lookupModuleEnv pit mod
   -- If the module does come from the home package, why do we look in the PIT as well?
   -- (a) In OneShot mode, even home-package modules accumulate in the PIT
   -- (b) Even in Batch (--make) mode, there is *one* case where a home-package
   --     module is in the PIT, namely GHC.Prim when compiling the base package.
   -- We could eliminate (b) if we wanted, by making GHC.Prim belong to a package
   -- of its own, but it doesn't seem worth the bother.

mainModIs :: HomeUnitEnv -> Module
mainModIs hue = mkHomeModule (expectJust "mainModIs" $ homeUnitEnv_home_unit hue) (mainModuleNameIs (homeUnitEnv_dflags hue))

-- | Retrieve the target code interpreter
--
-- Fails if no target code interpreter is available
hscInterp :: HscEnv -> Interp
hscInterp hsc_env = case hsc_interp hsc_env of
   Nothing -> throw (InstallationError "Couldn't find a target code interpreter. Try with -fexternal-interpreter")
   Just i  -> i

-- | Update the LogFlags of the Log in hsc_logger from the DynFlags in
-- hsc_dflags. You need to call this when DynFlags are modified.
hscUpdateLoggerFlags :: HscEnv -> HscEnv
hscUpdateLoggerFlags h = h
  { hsc_logger = setLogFlags (hsc_logger h) (initLogFlags (hsc_dflags h)) }

-- | Update Flags
hscUpdateFlags :: (DynFlags -> DynFlags) -> HscEnv -> HscEnv
hscUpdateFlags f h = hscSetFlags (f (hsc_dflags h)) h

-- | Set Flags
hscSetFlags :: HasDebugCallStack => DynFlags -> HscEnv -> HscEnv
hscSetFlags dflags h =
  hscUpdateLoggerFlags $ h { hsc_dflags = dflags
                           , hsc_unit_env = setFlags dflags (hsc_unit_env h) }

-- See Note [Multiple Home Units]
hscSetActiveHomeUnit :: HasDebugCallStack => HomeUnit -> HscEnv -> HscEnv
hscSetActiveHomeUnit home_unit = hscSetActiveUnitId (homeUnitId home_unit)

hscSetActiveUnitId :: HasDebugCallStack => UnitId -> HscEnv -> HscEnv
hscSetActiveUnitId uid e = e
  { hsc_unit_env = ue_setActiveUnit uid (hsc_unit_env e)
  , hsc_dflags = UnitEnv.unitFlags uid (hsc_unit_env e)  }

hscActiveUnitId :: HscEnv -> UnitId
hscActiveUnitId e = ue_currentUnit (hsc_unit_env e)

-- | Discard the contents of the InteractiveContext, but keep the DynFlags and
-- the loaded plugins.  It will also keep ic_int_print and ic_monad if their
-- names are from external packages.
discardIC :: HscEnv -> HscEnv
discardIC hsc_env
  = hsc_env { hsc_IC = empty_ic { ic_int_print = new_ic_int_print
                                , ic_monad     = new_ic_monad
                                , ic_plugins   = old_plugins
                                } }
  where
  -- Force the new values for ic_int_print and ic_monad to avoid leaking old_ic
  !new_ic_int_print = keep_external_name ic_int_print
  !new_ic_monad = keep_external_name ic_monad
  !old_plugins = ic_plugins old_ic
  dflags = ic_dflags old_ic
  old_ic = hsc_IC hsc_env
  empty_ic = emptyInteractiveContext dflags
  keep_external_name ic_name
    | nameIsFromExternalPackage home_unit old_name = old_name
    | otherwise = ic_name empty_ic
    where
    home_unit = hsc_home_unit hsc_env
    old_name = ic_name old_ic

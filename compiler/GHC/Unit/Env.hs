{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
-- ROMES:TODO: UnitEnv is a clean interface to query about all units loaded,
-- EPS or otherwise.
-- ROMES:TODO: CLEAN!!!!
module GHC.Unit.Env
    ( UnitEnv (..)
    , initUnitEnv
    , ueEPS -- Not really needed, get directly type families and rule base!
    , unsafeGetHomeUnit
    , updateHug
    , updateHpt_lazy
    , updateHpt
    -- * Unit Env helper functions
    , ue_units
    , ue_currentHomeUnitEnv

    -- Stuff used by Backpack could be simplifeid ehre
    , ue_setUnits
    , ue_unit_dbs
    , ue_all_home_unit_ids
    , ue_setUnitDbs
    , ue_hpt

    , ue_homeUnit
    , ue_setFlags
    , ue_setActiveUnit
    , ue_currentUnit
    , ue_findHomeUnitEnv
    , ue_updateHomeUnitEnv
    , ue_unitHomeUnit
    , ue_unitFlags
    , ue_renameUnitId
    , ue_transitiveHomeDeps
    -- * HomeUnitEnv
    , HomeUnitGraph
    , HomeUnitEnv (..)
    , mkHomeUnitEnv
    , lookupHugByModule
    , hugElts
    , lookupHug
    , addHomeModInfoToHug
    -- * UnitEnvGraph
    , UnitEnvGraph (..)
    , UnitEnvGraphKey
    , unitEnv_new
    , unitEnv_singleton
    , unitEnv_map
    , unitEnv_lookup_maybe
    , unitEnv_lookup
    , unitEnv_keys
    , unitEnv_elts
    , unitEnv_hpts
    , unitEnv_foldWithKey
    , unitEnv_mapWithKey
    -- * Invariants
    , assertUnitEnvInvariant
    -- * Preload units info
    , preloadUnitsInfo
    , preloadUnitsInfo'
    -- * Home Module functions
    , isUnitEnvInstalledModule )
where

import GHC.Prelude

import GHC.Unit.External
import GHC.Unit.State
import GHC.Unit.Home
import GHC.Unit.Types
import GHC.Unit.Home.ModInfo
import GHC.Unit.Home.PackageTable
import GHC.Unit.Home.Graph

import GHC.Platform
import GHC.Settings
import GHC.Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Utils.Misc (HasDebugCallStack)
import GHC.Driver.DynFlags
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Unit.Module.ModIface
import GHC.Unit.Module
import qualified Data.Set as Set

data UnitEnv = UnitEnv
    { ue_eps :: {-# UNPACK #-} !ExternalUnitCache
        -- ^ Information about the currently loaded external packages.
        -- This is mutable because packages will be demand-loaded during
        -- a compilation run as required.

    , ue_current_unit    :: UnitId

    , ue_home_unit_graph :: !HomeUnitGraph
        -- See Note [Multiple Home Units]

    , ue_platform  :: !Platform
        -- ^ Platform

    , ue_namever   :: !GhcNameVersion
        -- ^ GHC name/version (used for dynamic library suffix)
    }

ueEPS :: UnitEnv -> IO ExternalPackageState
ueEPS = eucEPS . ue_eps

initUnitEnv :: UnitId -> HomeUnitGraph -> GhcNameVersion -> Platform -> IO UnitEnv
initUnitEnv cur_unit hug namever platform = do
  eps <- initExternalUnitCache
  return $ UnitEnv
    { ue_eps             = eps
    , ue_home_unit_graph = hug
    , ue_current_unit    = cur_unit
    , ue_platform        = platform
    , ue_namever         = namever
    }

-- | Get home-unit
--
-- Unsafe because the home-unit may not be set
unsafeGetHomeUnit :: UnitEnv -> HomeUnit
unsafeGetHomeUnit ue = ue_unsafeHomeUnit ue

updateHpt_lazy :: (HomePackageTable -> HomePackageTable) -> UnitEnv -> UnitEnv
updateHpt_lazy = ue_updateHPT_lazy

updateHpt :: (HomePackageTable -> HomePackageTable) -> UnitEnv -> UnitEnv
updateHpt = ue_updateHPT

updateHug :: (HomeUnitGraph -> HomeUnitGraph) -> UnitEnv -> UnitEnv
updateHug = ue_updateHUG

ue_transitiveHomeDeps :: UnitId -> UnitEnv -> [UnitId]
ue_transitiveHomeDeps uid unit_env = Set.toList (loop Set.empty [uid])
  where
    loop acc [] = acc
    loop acc (uid:uids)
      | uid `Set.member` acc = loop acc uids
      | otherwise =
        let hue = homeUnitDepends (homeUnitEnv_units (ue_findHomeUnitEnv uid unit_env))
        in loop (Set.insert uid acc) (hue ++ uids)


-- -----------------------------------------------------------------------------
-- Extracting information from the packages in scope

-- Many of these functions take a list of packages: in those cases,
-- the list is expected to contain the "dependent packages",
-- i.e. those packages that were found to be depended on by the
-- current module/program.  These can be auto or non-auto packages, it
-- doesn't really matter.  The list is always combined with the list
-- of preload (command-line) packages to determine which packages to
-- use.

-- | Lookup 'UnitInfo' for every preload unit from the UnitState, for every unit
-- used to instantiate the home unit, and for every unit explicitly passed in
-- the given list of UnitId.
preloadUnitsInfo' :: UnitEnv -> [UnitId] -> MaybeErr UnitErr [UnitInfo]
preloadUnitsInfo' unit_env ids0 = all_infos
  where
    unit_state = ue_units unit_env
    ids      = ids0 ++ inst_ids
    inst_ids = case ue_homeUnit unit_env of
      Nothing -> []
      Just home_unit
       -- An indefinite package will have insts to HOLE,
       -- which is not a real package. Don't look it up.
       -- Fixes #14525
       | isHomeUnitIndefinite home_unit -> []
       | otherwise -> map (toUnitId . moduleUnit . snd) (homeUnitInstantiations home_unit)
    pkg_map = unitInfoMap unit_state
    preload = preloadUnits unit_state

    all_pkgs  = closeUnitDeps' pkg_map preload (ids `zip` repeat Nothing)
    all_infos = map (unsafeLookupUnitId unit_state) <$> all_pkgs


-- | Lookup 'UnitInfo' for every preload unit from the UnitState and for every
-- unit used to instantiate the home unit.
preloadUnitsInfo :: UnitEnv -> MaybeErr UnitErr [UnitInfo]
preloadUnitsInfo unit_env = preloadUnitsInfo' unit_env []

-- | Test if the module comes from the home unit
isUnitEnvInstalledModule :: UnitEnv -> InstalledModule -> Bool
isUnitEnvInstalledModule ue m = maybe False (`isHomeInstalledModule` m) hu
  where
    hu = ue_unitHomeUnit_maybe (moduleUnit m) ue

-- -------------------------------------------------------
-- Operations on arbitrary elements of the home unit graph
-- -------------------------------------------------------

ue_findHomeUnitEnv_maybe :: UnitId -> UnitEnv -> Maybe HomeUnitEnv
ue_findHomeUnitEnv_maybe uid e =
  unitEnv_lookup_maybe uid (ue_home_unit_graph e)

ue_findHomeUnitEnv :: HasDebugCallStack => UnitId -> UnitEnv -> HomeUnitEnv
ue_findHomeUnitEnv uid e = case unitEnv_lookup_maybe uid (ue_home_unit_graph e) of
  Nothing -> pprPanic "Unit unknown to the internal unit environment"
              $  text "unit (" <> ppr uid <> text ")"
              $$ pprUnitEnvGraph e
  Just hue -> hue

-- -------------------------------------------------------
-- Query and modify UnitState in HomeUnitEnv
-- -------------------------------------------------------

ue_units :: HasDebugCallStack => UnitEnv -> UnitState
ue_units = homeUnitEnv_units . ue_currentHomeUnitEnv

ue_unit_dbs :: UnitEnv ->  Maybe [UnitDatabase UnitId]
ue_unit_dbs = homeUnitEnv_unit_dbs . ue_currentHomeUnitEnv

-- -------------------------------------------------------
-- Query and modify Home Package Table in HomeUnitEnv
-- -------------------------------------------------------

ue_hpt :: HasDebugCallStack => UnitEnv -> HomePackageTable
ue_hpt = homeUnitEnv_hpt . ue_currentHomeUnitEnv

ue_updateHPT_lazy :: HasDebugCallStack => (HomePackageTable -> HomePackageTable) -> UnitEnv -> UnitEnv
ue_updateHPT_lazy f e = ue_updateUnitHPT_lazy f (ue_currentUnit e) e

ue_updateHPT :: HasDebugCallStack => (HomePackageTable -> HomePackageTable) -> UnitEnv -> UnitEnv
ue_updateHPT f e = ue_updateUnitHPT f (ue_currentUnit e) e

ue_updateHUG :: HasDebugCallStack => (HomeUnitGraph -> HomeUnitGraph) -> UnitEnv -> UnitEnv
ue_updateHUG f e = ue_updateUnitHUG f e

ue_updateUnitHPT_lazy :: HasDebugCallStack => (HomePackageTable -> HomePackageTable) -> UnitId -> UnitEnv -> UnitEnv
ue_updateUnitHPT_lazy f uid ue_env = ue_updateHomeUnitEnv update uid ue_env
  where
    update unitEnv = unitEnv { homeUnitEnv_hpt = f $ homeUnitEnv_hpt unitEnv }

ue_updateUnitHPT :: HasDebugCallStack => (HomePackageTable -> HomePackageTable) -> UnitId -> UnitEnv -> UnitEnv
ue_updateUnitHPT f uid ue_env = ue_updateHomeUnitEnv update uid ue_env
  where
    update unitEnv =
      let !res = f $ homeUnitEnv_hpt unitEnv
      in unitEnv { homeUnitEnv_hpt = res }

ue_updateUnitHUG :: HasDebugCallStack => (HomeUnitGraph -> HomeUnitGraph) -> UnitEnv -> UnitEnv
ue_updateUnitHUG f ue_env = ue_env { ue_home_unit_graph = f (ue_home_unit_graph ue_env)}

-- -------------------------------------------------------
-- Query and modify DynFlags in HomeUnitEnv
-- -------------------------------------------------------

ue_setFlags :: HasDebugCallStack => DynFlags -> UnitEnv -> UnitEnv
ue_setFlags dflags ue_env = ue_setUnitFlags (ue_currentUnit ue_env) dflags ue_env

ue_setUnitFlags :: HasDebugCallStack => UnitId -> DynFlags -> UnitEnv -> UnitEnv
ue_setUnitFlags uid dflags e =
  ue_updateUnitFlags (const dflags) uid e

ue_unitFlags :: HasDebugCallStack => UnitId -> UnitEnv -> DynFlags
ue_unitFlags uid ue_env = homeUnitEnv_dflags $ ue_findHomeUnitEnv uid ue_env

ue_updateUnitFlags :: HasDebugCallStack => (DynFlags -> DynFlags) -> UnitId -> UnitEnv -> UnitEnv
ue_updateUnitFlags f uid e = ue_updateHomeUnitEnv update uid e
  where
    update hue = hue { homeUnitEnv_dflags = f $ homeUnitEnv_dflags hue }

-- -------------------------------------------------------
-- Query and modify home units in HomeUnitEnv
-- -------------------------------------------------------

ue_homeUnit :: UnitEnv -> Maybe HomeUnit
ue_homeUnit = homeUnitEnv_home_unit . ue_currentHomeUnitEnv

ue_unsafeHomeUnit :: UnitEnv -> HomeUnit
ue_unsafeHomeUnit ue = case ue_homeUnit ue of
  Nothing -> panic "unsafeGetHomeUnit: No home unit"
  Just h  -> h

ue_unitHomeUnit_maybe :: UnitId -> UnitEnv -> Maybe HomeUnit
ue_unitHomeUnit_maybe uid ue_env =
  homeUnitEnv_unsafeHomeUnit <$> (ue_findHomeUnitEnv_maybe uid ue_env)

ue_unitHomeUnit :: UnitId -> UnitEnv -> HomeUnit
ue_unitHomeUnit uid ue_env = homeUnitEnv_unsafeHomeUnit $ ue_findHomeUnitEnv uid ue_env

ue_all_home_unit_ids :: UnitEnv -> Set.Set UnitId
ue_all_home_unit_ids = unitEnv_keys . ue_home_unit_graph
-- -------------------------------------------------------
-- Query and modify the currently active unit
-- -------------------------------------------------------

ue_currentHomeUnitEnv :: HasDebugCallStack => UnitEnv -> HomeUnitEnv
ue_currentHomeUnitEnv e =
  case ue_findHomeUnitEnv_maybe (ue_currentUnit e) e of
    Just unitEnv -> unitEnv
    Nothing -> pprPanic "packageNotFound" $
      (ppr $ ue_currentUnit e) $$ ppr (ue_home_unit_graph e)

ue_setActiveUnit :: UnitId -> UnitEnv -> UnitEnv
ue_setActiveUnit u ue_env = assertUnitEnvInvariant $ ue_env
  { ue_current_unit = u
  }

ue_currentUnit :: UnitEnv -> UnitId
ue_currentUnit = ue_current_unit


ue_updateHomeUnitEnv :: (HomeUnitEnv -> HomeUnitEnv) -> UnitId -> UnitEnv -> UnitEnv
ue_updateHomeUnitEnv f uid e = e
  { ue_home_unit_graph = unitEnv_adjust f uid $ ue_home_unit_graph e
  }


-- | Rename a unit id in the internal unit env.
--
-- @'ue_renameUnitId' oldUnit newUnit UnitEnv@, it is assumed that the 'oldUnit' exists in the map,
-- otherwise we panic.
-- The 'DynFlags' associated with the home unit will have its field 'homeUnitId' set to 'newUnit'.
ue_renameUnitId :: HasDebugCallStack => UnitId -> UnitId -> UnitEnv -> UnitEnv
ue_renameUnitId oldUnit newUnit unitEnv = case ue_findHomeUnitEnv_maybe oldUnit unitEnv of
  Nothing ->
    pprPanic "Tried to rename unit, but it didn't exist"
              $ text "Rename old unit \"" <> ppr oldUnit <> text "\" to \""<> ppr newUnit <> text "\""
              $$ nest 2 (pprUnitEnvGraph unitEnv)
  Just oldEnv ->
    let
      activeUnit :: UnitId
      !activeUnit = if ue_currentUnit unitEnv == oldUnit
                then newUnit
                else ue_currentUnit unitEnv

      newInternalUnitEnv = oldEnv
        { homeUnitEnv_dflags = (homeUnitEnv_dflags oldEnv)
            { homeUnitId_ = newUnit
            }
        }
    in
    unitEnv
      { ue_current_unit = activeUnit
      , ue_home_unit_graph =
          unitEnv_insert newUnit newInternalUnitEnv
          $ unitEnv_delete oldUnit
          $ ue_home_unit_graph unitEnv
          }

-- ---------------------------------------------
-- Asserts to enforce invariants for the UnitEnv
-- ---------------------------------------------

assertUnitEnvInvariant :: HasDebugCallStack => UnitEnv -> UnitEnv
assertUnitEnvInvariant u =
  if ue_current_unit u `unitEnv_member` ue_home_unit_graph u
    then u
    else pprPanic "invariant" (ppr (ue_current_unit u) $$ ppr (ue_home_unit_graph u))

-- -----------------------------------------------------------------------------
-- Pretty output functions
-- -----------------------------------------------------------------------------

pprUnitEnvGraph :: UnitEnv -> SDoc
pprUnitEnvGraph env = text "pprInternalUnitMap"
  $$ nest 2 (pprHomeUnitGraph $ ue_home_unit_graph env)

{-
Note [Multiple Home Units]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The basic idea of multiple home units is quite simple. Instead of allowing one
home unit, you can multiple home units

The flow:

1. Dependencies between units are specified between each other in the normal manner,
   a unit is identified by the -this-unit-id flag and dependencies specified by
   the normal -package-id flag.
2. Downsweep is augmented to know to know how to look for dependencies in any home unit.
3. The rest of the compiler is modified appropriately to offset paths to the right places.
4. --make mode can parallelise between home units and multiple units are allowed to produce linkables.

Closure Property
----------------

You must perform a clean cut of the dependency graph.

> Any dependency which is not a home unit must not (transitively) depend on a home unit.

For example, if you have three packages p, q and r, then if p depends on q which
depends on r then it is illegal to load both p and r as home units but not q,
because q is a dependency of the home unit p which depends on another home unit r.

Offsetting Paths
----------------

The main complication to the implementation is to do with offsetting paths appropriately.
For a long time it has been assumed that GHC will execute in the top-directory for a unit,
normally where the .cabal file is and all paths are interpreted relative to there.
When you have multiple home units then it doesn't make sense to pick one of these
units to choose as the base-unit, and you can't robustly change directories when
using parallelism.

Therefore there is an option `-working-directory`, which tells GHC where the relative
paths for each unit should be interpreted relative to. For example, if you specify
`-working-dir a -ib`, then GHC will offset the relative path `b`, by `a`, and look for
source files in `a/b`. The same thing happens for any path passed on the command line.

A non-exhaustive list is

* -i
* -I
* -odir/-hidir/-outputdir/-stubdir/-hiedir
* Target files passed on the command line

There is also a template-haskell function, makeRelativeToProject, which uses the `-working-directory` option
in order to allow users to offset their own relative paths.

-}

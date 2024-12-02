-- | A 'HomeUnitGraph' (HUG) collects information about all the home units.
-- Crucially, each node in a 'HomeUnitGraph' includes a 'HomePackageTable'.
--
-- Often, we don't want to query just a single 'HomePackageTable', but rather all
-- 'HomePackageTable's of all home units.
--
-- This module is responsible for maintaining this bridge between querying all
-- home units vs querying the home package table directly. Think 'lookupHug' vs
-- 'lookupHpt', 'hugAllInstances' vs 'hptAllInstances', where the @hug@ version
-- replies with information from all home units, and the @hpt@ version with
-- information pertaining to a single home unit.
--
-- Meant to be imported qualified as @HUG@.
-- Example usage:
--
-- @
-- import GHC.Unit.Home.Graph (HomeUnitGraph, HomeUnitEnv)
-- import qualified GHC.Unit.Home.Graph as HUG
-- usage = ... HUG.insertHug hug uid modname modinfo ...
-- @
module GHC.Unit.Home.Graph
  ( HomeUnitGraph
  , HomeUnitEnv(..) -- romes: validate if should expose internals of this
  , mkHomeUnitEnv

  -- * Operations
  , addHomeModInfoToHug
  , updateUnitFlags
  , renameUnitId
  , allUnits

  -- ** Lookups
  , lookupHug
  , lookupHugByModule
  , lookupHugUnit

  -- ** Reachability
  , transitiveHomeDeps

  -- * Very important
  , famInstances
  , allInstances
  , completeSigs
  , rulesBelow
  , annsBelow
  , instancesBelow

  -- * Printing
  , pprHomeUnitGraph
  , pprHomeUnitEnv

  -- * Auxiliary structure
  , UnitEnvGraph
  , unitEnv_lookup_maybe
  , unitEnv_foldWithKey
  , unitEnv_singleton
  , unitEnv_adjust
  ) where

import GHC.Prelude

import GHC.Driver.DynFlags
import GHC.Unit.Home
import GHC.Unit.Home.ModInfo
import GHC.Unit.Home.PackageTable
import GHC.Unit.Module
import GHC.Unit.Module.ModIface
import GHC.Unit.State
import GHC.Utils.Outputable
import GHC.Utils.Panic

import GHC.Core.FamInstEnv

import Data.Map.Strict (Map)
import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Data.Maybe

import GHC.Core.Rules
import GHC.Types.Annotations
import GHC.Types.CompleteMatch
import GHC.Core.InstEnv
import GHC.Core.FamInstEnv

--------------------------------------------------------------------------------
-- TODO
--------------------------------------------------------------------------------
-- WE MAY WANT TO CACHE SOME OF THESE AS WE BUILD UP THE HPT, to make these
-- queries O(1). But it's kind of hard because they wouldn't be rehydrated!!!!!
-- Then we'd have the HPT itself rehydrated, but the cached fields with
-- bad references.

-- | Get all 'CompleteMatches' (arising from COMPLETE pragmas) present across
-- all home units.
completeSigs :: HomeUnitGraph -> IO CompleteMatches
completeSigs = undefined

-- | Get annotations from all modules "below" this one (in the dependency
-- sense) within the home units. If the module is @Nothing@, returns /all/
-- annotations in the home units.
annsBelow :: HomeUnitGraph -> Maybe (UnitId, ModuleNameWithIsBoot) -> IO AnnEnv
annsBelow = undefined

---- | Get rules from modules "below" this one (in the dependency sense) within
--the home units.
rulesBelow :: HomeUnitGraph -> UnitId -> ModuleNameWithIsBoot -> IO RuleBase
rulesBelow = undefined

-- | Find instances visible from the given set of imports
instancesBelow :: HomeUnitGraph -> UnitId -> ModuleNameWithIsBoot -> IO (InstEnv, [FamInst])
instancesBelow = undefined

-- | Find all the instance declarations (of classes and families) from
-- the Home Package Table filtered by the provided predicate function.
-- Used in @tcRnImports@, to select the instances that are in the
-- transitive closure of imports from the currently compiled module.
allInstances :: HomeUnitGraph -> IO (InstEnv, [FamInst])
allInstances = undefined

-- ROMES:TODO:
famInstances :: HomeUnitGraph -> IO (ModuleEnv FamInstEnv)
famInstances = undefined
   -- mkModuleEnv [ (hmiModule hmi, hmiFamInstEnv hmi)
   --                             | hpt <- unitEnv_hpts hug
   --                             , hmi <- eltsHpt hpt ]

--              ; hmiModule     = mi_module . hm_iface
--              ; hmiFamInstEnv = extendFamInstEnvList emptyFamInstEnv
--                                . md_fam_insts . hm_details

--------------------------------------------------------------------------------
-- OK?
--------------------------------------------------------------------------------

type HomeUnitGraph = UnitEnvGraph HomeUnitEnv

data HomeUnitEnv = HomeUnitEnv
  { homeUnitEnv_units     :: !UnitState
      -- ^ External units

  , homeUnitEnv_unit_dbs :: !(Maybe [UnitDatabase UnitId])
      -- ^ Stack of unit databases for the target platform.
      --
      -- This field is populated with the result of `initUnits`.
      --
      -- 'Nothing' means the databases have never been read from disk.
      --
      -- Usually we don't reload the databases from disk if they are
      -- cached, even if the database flags changed!

  , homeUnitEnv_dflags :: DynFlags
    -- ^ The dynamic flag settings
  , homeUnitEnv_hpt :: HomePackageTable
    -- ^ The home package table describes already-compiled
    -- home-package modules, /excluding/ the module we
    -- are compiling right now.
    -- (In one-shot mode the current module is the only
    -- home-package module, so homeUnitEnv_hpt is empty.  All other
    -- modules count as \"external-package\" modules.
    -- However, even in GHCi mode, hi-boot interfaces are
    -- demand-loaded into the external-package table.)
    --
    -- 'homeUnitEnv_hpt' is not mutable because we only demand-load
    -- external packages; the home package is eagerly
    -- loaded, module by module, by the compilation manager.
    --
    -- The HPT may contain modules compiled earlier by @--make@
    -- but not actually below the current module in the dependency
    -- graph.
    --
    -- (This changes a previous invariant: changed Jan 05.)

  , homeUnitEnv_home_unit :: !(Maybe HomeUnit)
    -- ^ Home-unit
  }

mkHomeUnitEnv :: UnitState -> Maybe [UnitDatabase UnitId] -> DynFlags -> HomePackageTable -> Maybe HomeUnit -> HomeUnitEnv
mkHomeUnitEnv us dbs dflags hpt home_unit = HomeUnitEnv
  { homeUnitEnv_units = us
  , homeUnitEnv_unit_dbs = dbs
  , homeUnitEnv_dflags = dflags
  , homeUnitEnv_hpt = hpt
  , homeUnitEnv_home_unit = home_unit
  }

--------------------------------------------------------------------------------
-- * Operations on HUG
--------------------------------------------------------------------------------

-- | Add an entry to the 'HomePackageTable' under the unit of that entry.
addHomeModInfoToHug :: HomeModInfo -> HomeUnitGraph -> IO HomeUnitGraph
addHomeModInfoToHug hmi hug =
  case unitEnv_lookup_maybe hmi_unit hug of
    Nothing -> pprPanic "addHomeInfoToHug" (ppr hmi_mod)
    Just hue -> do
      hpt <- addHomeModInfoToHpt hmi (homeUnitEnv_hpt hue)
      return $
        unitEnv_insert hmi_unit hue{homeUnitEnv_hpt=hpt} hug
  where
    hmi_mod :: Module
    hmi_mod = mi_module (hm_iface hmi)

    hmi_unit = toUnitId (moduleUnit hmi_mod)
    _hmi_mn   = moduleName hmi_mod


-- | Set the 'DynFlags' of the 'HomeUnitEnv' for unit in the 'HomeModuleGraph'
updateUnitFlags :: UnitId -> (DynFlags -> DynFlags) -> HomeUnitGraph -> HomeUnitGraph
updateUnitFlags uid f = undefined

-- | Rename a unit id in the 'HomeUnitGraph'
--
-- @'renameUnitId' oldUnit newUnit hug@, if @oldUnit@ is not found in @hug@, returns 'Nothing'.
-- If it exists, the result maps @newUnit@ to the 'HomeUnitEnv' of the
-- @oldUnit@ (and @oldUnit@ is removed from @hug@)
renameUnitId :: UnitId -> UnitId -> HomeUnitGraph -> Maybe HomeUnitGraph
renameUnitId oldUnit newUnit hug = case unitEnv_lookup_maybe oldUnit hug of
  Nothing -> Nothing
  Just oldHue -> pure $
    unitEnv_insert newUnit oldHue $
    unitEnv_delete oldUnit hug

-- | Retrieve all 'UnitId's of units in the 'HomeUnitGraph'.
allUnits :: HomeUnitGraph -> Set.Set UnitId
allUnits = unitEnv_keys

--------------------------------------------------------------------------------
-- ** Reachability
--------------------------------------------------------------------------------

-- | Compute the transitive closure of a unit in the 'HomeUnitGraph'.
-- If the argument unit is not present in the graph returns Nothing.
transitiveHomeDeps :: UnitId -> HomeUnitGraph -> Maybe [UnitId]
-- todo: is it worth it caching this computation further?
transitiveHomeDeps uid hug = case lookupHugUnit uid hug of
  Nothing -> Nothing
  Just hue -> Just $
    Set.toList (loop (Set.singleton uid) (homeUnitDepends (homeUnitEnv_units hue)))
    where
      loop acc [] = acc
      loop acc (uid:uids)
        | uid `Set.member` acc = loop acc uids
        | otherwise =
          let hue = homeUnitDepends
                    . homeUnitEnv_units
                    . expectJust "transitiveHomeDeps: homeUnitDepends of unit not found in hug"
                    $ lookupHugUnit uid hug
          in loop (Set.insert uid acc) (hue ++ uids)

--------------------------------------------------------------------------------
-- ** Lookups
--------------------------------------------------------------------------------

-- | Lookup the 'HomeModInfo' of a 'Module' in the 'HomeUnitGraph' given its
-- 'UnitId' and 'ModuleName' (via the 'HomePackageTable' of the corresponding unit)
lookupHug :: HomeUnitGraph -> UnitId -> ModuleName -> IO (Maybe HomeModInfo)
lookupHug hug uid mod = do
  case unitEnv_lookup_maybe uid hug of
    -- Really, here we want "lookup HPT" rather than unitEnvLookup
    Nothing -> pure Nothing
    Just hue -> lookupHpt (homeUnitEnv_hpt hue) mod 

-- | Lookup the 'HomeModInfo' of a 'Module' in the 'HomeUnitGraph' (via the 'HomePackageTable' of the corresponding unit)
lookupHugByModule :: Module -> HomeUnitGraph -> IO (Maybe HomeModInfo)
lookupHugByModule mod hug
  | otherwise = do
      case unitEnv_lookup_maybe (toUnitId $ moduleUnit mod) hug of
        Nothing -> pure Nothing
        Just env -> lookupHptByModule (homeUnitEnv_hpt env) mod

-- | Lookup a 'HomeUnitEnv' by 'UnitId' in a 'HomeUnitGraph'
lookupHugUnit :: UnitId -> HomeUnitGraph -> Maybe HomeUnitEnv
lookupHugUnit = unitEnv_lookup_maybe

--------------------------------------------------------------------------------
-- TODO:!!!!
--------------------------------------------------------------------------------

homeUnitEnv_unsafeHomeUnit :: HomeUnitEnv -> HomeUnit
homeUnitEnv_unsafeHomeUnit hue = case homeUnitEnv_home_unit hue of
  Nothing -> panic "homeUnitEnv_unsafeHomeUnit: No home unit"
  Just h  -> h

hugElts :: HomeUnitGraph -> [(UnitId, HomeUnitEnv)]
hugElts hug = unitEnv_elts hug

updateHueHpt :: (HomePackageTable -> HomePackageTable) -> HomeUnitEnv -> HomeUnitEnv
updateHueHpt f hue =
  let !hpt =  f (homeUnitEnv_hpt hue)
  in hue { homeUnitEnv_hpt = hpt }

type UnitEnvGraphKey = UnitId

newtype UnitEnvGraph v = UnitEnvGraph
  { unitEnv_graph :: Map UnitEnvGraphKey v
  } deriving (Functor, Foldable, Traversable)

unitEnv_insert :: UnitEnvGraphKey -> v -> UnitEnvGraph v -> UnitEnvGraph v
unitEnv_insert unitId env unitEnv = unitEnv
  { unitEnv_graph = Map.insert unitId env (unitEnv_graph unitEnv)
  }

unitEnv_delete :: UnitEnvGraphKey -> UnitEnvGraph v -> UnitEnvGraph v
unitEnv_delete uid unitEnv =
    unitEnv
      { unitEnv_graph = Map.delete uid (unitEnv_graph unitEnv)
      }

unitEnv_adjust :: (v -> v) -> UnitEnvGraphKey -> UnitEnvGraph v -> UnitEnvGraph v
unitEnv_adjust f uid unitEnv = unitEnv
  { unitEnv_graph = Map.adjust f uid (unitEnv_graph unitEnv)
  }

unitEnv_alter :: (Maybe v -> Maybe v) -> UnitEnvGraphKey -> UnitEnvGraph v -> UnitEnvGraph v
unitEnv_alter f uid unitEnv = unitEnv
  { unitEnv_graph = Map.alter f uid (unitEnv_graph unitEnv)
  }

unitEnv_mapWithKey :: (UnitEnvGraphKey -> v -> b) -> UnitEnvGraph v -> UnitEnvGraph b
unitEnv_mapWithKey f (UnitEnvGraph u) = UnitEnvGraph $ Map.mapWithKey f u

unitEnv_new :: Map UnitEnvGraphKey v -> UnitEnvGraph v
unitEnv_new m =
  UnitEnvGraph
    { unitEnv_graph = m
    }

unitEnv_singleton :: UnitEnvGraphKey -> v -> UnitEnvGraph v
unitEnv_singleton active m = UnitEnvGraph
  { unitEnv_graph = Map.singleton active m
  }

unitEnv_map :: (v -> v) -> UnitEnvGraph v -> UnitEnvGraph v
unitEnv_map f m = m { unitEnv_graph = Map.map f (unitEnv_graph m)}

unitEnv_member :: UnitEnvGraphKey -> UnitEnvGraph v -> Bool
unitEnv_member u env = Map.member u (unitEnv_graph env)

unitEnv_lookup_maybe :: UnitEnvGraphKey -> UnitEnvGraph v -> Maybe v
unitEnv_lookup_maybe u env = Map.lookup u (unitEnv_graph env)

unitEnv_lookup :: UnitEnvGraphKey -> UnitEnvGraph v -> v
unitEnv_lookup u env = fromJust $ unitEnv_lookup_maybe u env

unitEnv_keys :: UnitEnvGraph v -> Set.Set UnitEnvGraphKey
unitEnv_keys env = Map.keysSet (unitEnv_graph env)

unitEnv_elts :: UnitEnvGraph v -> [(UnitEnvGraphKey, v)]
unitEnv_elts env = Map.toList (unitEnv_graph env)

unitEnv_hpts :: UnitEnvGraph HomeUnitEnv -> [HomePackageTable]
unitEnv_hpts env = map homeUnitEnv_hpt (Map.elems (unitEnv_graph env))

unitEnv_foldWithKey :: (b -> UnitEnvGraphKey -> a -> b) -> b -> UnitEnvGraph a -> b
unitEnv_foldWithKey f z (UnitEnvGraph g)= Map.foldlWithKey' f z g

-- unitEnv_union :: (a -> a -> a) -> UnitEnvGraph a -> UnitEnvGraph a -> UnitEnvGraph a
-- unitEnv_union f (UnitEnvGraph env1) (UnitEnvGraph env2) = UnitEnvGraph (Map.unionWith f env1 env2)

--------------------------------------------------------------------------------
-- * Utilities
--------------------------------------------------------------------------------

pprHomeUnitGraph :: HomeUnitGraph -> IO SDoc
pprHomeUnitGraph unitEnv = do
  docs <- mapM (\(k, v) -> pprHomeUnitEnv k v) $ Map.assocs $ unitEnv_graph unitEnv
  return $ vcat docs

pprHomeUnitEnv :: UnitId -> HomeUnitEnv -> IO SDoc
pprHomeUnitEnv uid env = do
  hptDoc <- pprHPT $ homeUnitEnv_hpt env
  return $
    ppr uid <+> text "(flags:" <+> ppr (homeUnitId_ $ homeUnitEnv_dflags env) <> text "," <+> ppr (fmap homeUnitId $ homeUnitEnv_home_unit env) <> text ")" <+> text "->"
    $$ nest 4 hptDoc


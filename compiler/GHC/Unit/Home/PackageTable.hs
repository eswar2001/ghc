{-# LANGUAGE LambdaCase #-}
-- | The 'HomePackageTable' (HPT) contains information about all modules that are part
-- of a home package. At its core, the information for each module is a
-- 'ModInfo'.
--
-- The HPT is a monotonically increasing data structure: it only ever gets
-- extended by inserting modules which are loaded and for which we discover the
-- information required to construct a 'ModInfo'.
--
-- There should only ever exist one single HPT for any given package. It's
-- crucial we don't accidentally leak HPTs (e.g. by filtering it, which used to
-- happen), so the HPT is mutable and only its reference should be shared.
-- This is alright because the modules don't change throughout compilation.
--
-- There are various types of queries the compiler needs to do about the
-- modules of a package, such as getting the available type class instances,
-- type family instances, rules, COMPLETE pragmas, etc...
--
-- Those queries are frequent and should be answered efficiently. That's why
-- the the HPT iteratively constructs a cache of some of these things (another
-- reason why it's useful its interface doesn't allow arbitrary updates).
--
-- Note that to answer some queries, such as which instances are available in the
-- scope of Module X, a 'ModuleGraph' is also needed. The 'ModuleGraph' is
-- constructed once and for all during downsweep. It describes the structure
-- and relationship between different modules in the home units, as opposed to
-- the HPT which stores information about each module, but not about their structure.
--
-- :::WARNING:::
-- If you intend to change this interface, consider carefully whether you are
-- exposing memory-leak footguns which may end up being misused in the compiler
-- eventually. For instance, if you really, really, end up needing a way to take
-- a snapshot of the IORef (think: do you really need to?), at least make
-- obvious in the name like `snapshotCopyHpt`.
--
-- Or, do you really need a function to traverse all modules in the HPT? It is
-- often better to keep the computation internal to this module, such as in
-- 'hptCollectDependencies'...
module GHC.Unit.Home.PackageTable
  (
    HomePackageTable
  , newHomePackageTable

    -- * Lookups in the HPT
  , lookupHptByModule
  , lookupHpt

    -- * Extending the HPT
  , addHomeModInfoToHpt

    -- * Queries about home modules
  , hptHasHoles
  , hptLastLoadedKey
  -- , hpt

    -- ** Transitive closure queries
    --
    -- | These are the queries which also require access to the 'ModuleGraph'
    -- which describes the structure of the modules, rather than being "global queries".
    -- Typically about the transitive closure

    -- * Traversal-based queries
  , hptCollectDependencies
  , hptCollectObjects
  , hptCollectModules

    -- * Utilities
  , pprHPT
  ) where

import GHC.Prelude
import GHC.Data.Maybe

import Data.List (sortOn)
import Data.Ord
import Data.IORef
import qualified Data.Set as Set

import GHC.Types.Unique
import GHC.Types.Unique.DFM
import GHC.Linker.Types
import GHC.Unit.Home.ModInfo
import GHC.Unit.Module
import GHC.Unit.Module.Deps
import GHC.Unit.Module.ModIface
import GHC.Utils.Outputable
import Control.Monad ((<$!>))

-- | Helps us find information about modules in the home package
data HomePackageTable = HPT {

    table :: IORef (DModuleNameEnv HomeModInfo),
    -- ^ Domain = modules in the home unit that have been fully compiled
    -- "home" unit id cached (implicit) here for convenience.
    --
    -- This is an IORef because the HPT musn't leak; We want to always augment
    -- it, and handle rehydration such that rehydrated modules point to the
    -- actual modules rather than to hs-boot files... Previously we did this by
    -- tying a knot on the lazy HPT, but this leaked the HPT, ...
    --
    -- The elements of this table may be updated (e.g. on rehydration).
    --
    -- ROMES:TODO: Explain

    hasHoles :: !Bool,
    -- ^ Does this HPT have any module with a hole?
    --
    -- A module with a hole will never change to not having a hole, and
    -- vice-versa. The table is monotonically increasing, so we needn't worry
    -- about the table 'HomeModInfo' updates. On insertions we must make sure to
    -- update this field (insertions can only be done through the API exposed).

    lastLoadedKey :: !(Maybe Unique)
    -- ^ What was the last module loaded into this HPT?
    --
    -- Like 'hasHoles', this is a cache that is updated with insertions and kept
    -- up to date with the monotonically increasing map in the IORef.
    --
    -- When renaming any 'RdrName', in 'unboundName'/'unboundNameX', there is a
    -- check which previously traversed all modules of the HTP to get the last inserted.
    -- Now, it's always cached in this field.


    -- Careful when caching things outside the mutable reference:
    -- Recall that rehydration will update the HMIs in the HPT reference, but
    -- things cached outside would still retain abstract references (e.g. to
    -- boot modules).
  }

-- romes:todo: use a MutableArray directly?
newHomePackageTable :: IO HomePackageTable
newHomePackageTable = do
  table <- newIORef emptyUDFM
  return HPT{table, hasHoles=False, lastLoadedKey=Nothing}

--------------------------------------------------------------------------------
-- * Lookups in the HPT
--------------------------------------------------------------------------------

lookupHptByModule :: HomePackageTable -> Module -> IO (Maybe HomeModInfo)
-- The HPT is indexed by ModuleName, not Module,
-- we must check for a hit on the right Module
lookupHptByModule hpt mod
  = lookupHpt hpt (moduleName mod) >>= pure . \case
      Just hm | mi_module (hm_iface hm) == mod -> Just hm
      _otherwise                               -> Nothing

--------------------------------------------------------------------------------
-- * Extending the HPT
--------------------------------------------------------------------------------

addHomeModInfoToHpt :: HomeModInfo -> HomePackageTable -> IO HomePackageTable
addHomeModInfoToHpt hmi hpt = addToHpt hpt (moduleName (mi_module (hm_iface hmi))) hmi

----------------------------------------------------------------------------------
---- * Queries
----------------------------------------------------------------------------------

-- | Is there any module in this HPT which has a backpack hole?
--
-- $O(1)$
hptHasHoles :: HomePackageTable -> Bool
hptHasHoles HPT{hasHoles} = hasHoles

-- | Returns the unique of the last home module added to the home package table
--
-- $O(1)$
hptLastLoadedKey :: HomePackageTable -> Maybe Unique
hptLastLoadedKey HPT{lastLoadedKey} = lastLoadedKey

---- | Find all the instance declarations (of classes and families) from
---- the Home Package Table filtered by the provided predicate function.
---- Used in @tcRnImports@, to select the instances that are in the
---- transitive closure of imports from the currently compiled module.
--hptAllInstances :: HscEnv -> (InstEnv, [FamInst])
--hptAllInstances hsc_env
--  = let (insts, famInsts) = unzip $ flip hptAllThings hsc_env $ \mod_info -> do
--                let details = hm_details mod_info
--                return (md_insts details, md_fam_insts details)
--    in (foldl' unionInstEnv emptyInstEnv insts, concat famInsts)

--hptCompleteSigs :: HscEnv -> CompleteMatches
--hptCompleteSigs = hptAllThings (md_complete_matches . hm_details)

--hptAllThings :: (HomeModInfo -> [a]) -> HscEnv -> [a]
--hptAllThings extract hsc_env = concatMap (concatHpt extract . homeUnitEnv_hpt . snd)
--                                (hugElts (hsc_HUG hsc_env))

----------------------------------------------------------------------------------
---- * Queries on Transitive Closure
----------------------------------------------------------------------------------

---- | Get rules from modules "below" this one (in the dependency sense)
--hptRules :: HscEnv -> UnitId -> ModuleNameWithIsBoot -> [CoreRule]
--hptRules = hptSomeThingsBelowUs (md_rules . hm_details) False

---- | Get annotations from modules "below" this one (in the dependency sense)
--hptAnns :: HscEnv -> Maybe (UnitId, ModuleNameWithIsBoot) -> [Annotation]
--hptAnns hsc_env (Just (uid, mn)) = hptSomeThingsBelowUs (md_anns . hm_details) False hsc_env uid mn
--hptAnns hsc_env Nothing = hptAllThings (md_anns . hm_details) hsc_env

---- | Find instances visible from the given set of imports
--hptInstancesBelow :: HscEnv -> UnitId -> ModuleNameWithIsBoot -> (InstEnv, [FamInst])
--hptInstancesBelow hsc_env uid mnwib =
--  let
--    mn = gwib_mod mnwib
--    (insts, famInsts) =
--        unzip $ hptSomeThingsBelowUs (\mod_info ->
--                                     let details = hm_details mod_info
--                                     -- Don't include instances for the current module
--                                     in if moduleName (mi_module (hm_iface mod_info)) == mn
--                                          then []
--                                          else [(md_insts details, md_fam_insts details)])
--                             True -- Include -hi-boot
--                             hsc_env
--                             uid
--                             mnwib
--  in (foldl' unionInstEnv emptyInstEnv insts, concat famInsts)

---- | Get things from modules "below" this one (in the dependency sense)
---- C.f Inst.hptInstances
--hptSomeThingsBelowUs :: (HomeModInfo -> [a]) -> Bool -> HscEnv -> UnitId -> ModuleNameWithIsBoot -> [a]
--hptSomeThingsBelowUs extract include_hi_boot hsc_env uid mn
--  | isOneShot (ghcMode (hsc_dflags hsc_env)) = []

--  | otherwise
--  = let hug = hsc_HUG hsc_env
--        mg  = hsc_mod_graph hsc_env
--    in
--    [ thing
--      -- "Finding each non-hi-boot module below me" maybe could be cached in the module
--      -- graph to avoid filtering the boots out of the transitive closure out
--      -- every time this is called
--    | (ModNodeKeyWithUid (GWIB { gwib_mod = mod, gwib_isBoot = is_boot }) mod_uid)
--          <- Set.toList (moduleGraphModulesBelow mg uid mn)
--    , include_hi_boot || (is_boot == NotBoot)

--        -- unsavoury: when compiling the base package with --make, we
--        -- sometimes try to look up RULES etc for GHC.Prim. GHC.Prim won't
--        -- be in the HPT, because we never compile it; it's in the EPT
--        -- instead. ToDo: clean up, and remove this slightly bogus filter:
--    , mod /= moduleName gHC_PRIM
--    , not (mod == gwib_mod mn && uid == mod_uid)

--        -- Look it up in the HPT
--    , let things = case lookupHug hug mod_uid mod of
--                    Just info -> extract info
--                    Nothing -> pprTrace "WARNING in hptSomeThingsBelowUs" msg mempty
--          msg = vcat [text "missing module" <+> ppr mod,
--                     text "When starting from"  <+> ppr mn,
--                     text "below:" <+> ppr (moduleGraphModulesBelow mg uid mn),
--                      text "Probable cause: out-of-date interface files"]
--                        -- This really shouldn't happen, but see #962
--    , thing <- things
--    ]

--------------------------------------------------------------------------------
-- * Traversal-based queries
--------------------------------------------------------------------------------

-- | Collect the immediate dependencies of all modules in the HPT into a Set.
-- The immediate dependencies are given by the iface as @'dep_direct_pkgs' . 'mi_deps'@.
--
-- $O(n)$ in the number of modules in the HPT.
hptCollectDependencies :: HomePackageTable -> IO (Set.Set UnitId)
hptCollectDependencies HPT{table} = do
  hpt <- readIORef table
  return $
    foldr (Set.union . dep_direct_pkgs . mi_deps . hm_iface) Set.empty hpt

-- | Collect the linkable object of all modules in the HPT.
-- The linkable objects are given by @'homeModInfoObject'@.
--
-- $O(n)$ in the number of modules in the HPT.
hptCollectObjects :: HomePackageTable -> IO [Linkable]
hptCollectObjects HPT{table} = do
  hpt <- readIORef table
  return $
    foldr ((:) . expectJust "collectObjects" . homeModInfoObject) [] hpt

-- | Collect all module ifaces in the HPT
--
-- $O(n)$ in the number of modules in the HPT.
hptCollectModules :: HomePackageTable -> IO [Module]
hptCollectModules HPT{table} = do
  hpt <- readIORef table
  return $
    foldr ((:) . mi_module . hm_iface) [] hpt

--------------------------------------------------------------------------------
-- * Utilities
--------------------------------------------------------------------------------

pprHPT :: HomePackageTable -> IO SDoc
-- A bit arbitrary for now
pprHPT HPT{table=hptr} = do
  hpt <- readIORef hptr
  return $!
    pprUDFM hpt $ \hms ->
      vcat [ ppr (mi_module (hm_iface hm))
           | hm <- hms ]

----------------------------------------------------------------------------------
-- ROMES:TODO: FROM HERE!!!
----------------------------------------------------------------------------------

-- | Constructs an empty HomePackageTable
-- emptyHomePackageTable :: HomePackageTable
-- emptyHomePackageTable  = emptyUDFM

lookupHpt :: HomePackageTable -> ModuleName -> IO (Maybe HomeModInfo)
lookupHpt HPT{table=hpt} mn = (`lookupUDFM` mn) <$!> readIORef hpt

-- LOOKS LIKE A VERY LONG FOOT GUN...
-- eltsHpt :: HomePackageTable -> [HomeModInfo]
-- eltsHpt = eltsUDFM

-- | Like @concatMap f . 'eltsHpt'@, but filters out all 'HomeModInfo' for which
-- @f@ returns the empty list before doing the sort inherent to 'eltsUDFM'.
-- concatHpt :: (HomeModInfo -> [a]) -> HomePackageTable -> [a]
-- concatHpt f = concat . eltsUDFM . mapMaybeUDFM g
--   where
--     g hmi = case f hmi of { [] -> Nothing; as -> Just as }

-- filterHpt :: (HomeModInfo -> Bool) -> HomePackageTable -> HomePackageTable
-- filterHpt = filterUDFM

-- allHpt :: (HomeModInfo -> Bool) -> HomePackageTable -> Bool
-- allHpt = allUDFM

-- anyHpt :: (HomeModInfo -> Bool) -> HomePackageTable -> Bool
-- anyHpt = anyUDFM

-- mapHpt :: (HomeModInfo -> HomeModInfo) -> HomePackageTable -> HomePackageTable
-- mapHpt = mapUDFM

-- delFromHpt :: HomePackageTable -> ModuleName -> HomePackageTable
-- delFromHpt = delFromUDFM

addToHpt :: HomePackageTable -> ModuleName -> HomeModInfo -> IO HomePackageTable
addToHpt HPT{table=hptr, hasHoles} mn hmi = do
  atomicModifyIORef' hptr (\hpt -> (addToUDFM hpt mn hmi, ()))
  return
    HPT{ table = hptr
       , hasHoles = hasHoles && isHoleModule (mi_semantic_module (hm_iface hmi))
       , lastLoadedKey = Just $! getUnique mn
       }

-- addListToHpt
--   :: HomePackageTable -> [(ModuleName, HomeModInfo)] -> HomePackageTable
-- addListToHpt = addListToUDFM

-- listToHpt :: [(ModuleName, HomeModInfo)] -> HomePackageTable
-- listToHpt = listToUDFM

-- listHMIToHpt :: [HomeModInfo] -> HomePackageTable
-- listHMIToHpt hmis =
--   listToHpt [(moduleName (mi_module (hm_iface hmi)), hmi) | hmi <- sorted_hmis]
--   where
--     -- Sort to put Non-boot things last, so they overwrite the boot interfaces
--     -- in the HPT, other than that, the order doesn't matter
--     sorted_hmis = sortOn (Down . mi_boot . hm_iface) hmis


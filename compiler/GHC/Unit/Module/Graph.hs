{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveTraversable #-}

module GHC.Unit.Module.Graph
   ( ModuleGraph
   , ModuleGraphNode(..)
   , nodeDependencies
   , emptyMG
   , mkModuleGraph
   , extendMG
   , extendMGInst
   , extendMG'
   , unionMG
   , isTemplateHaskellOrQQNonBoot
   , isExplicitStageMS
   , filterToposortToModules
   , mapMG
   , mgModSummaries
   , mgModSummaries'
   , mgLookupModule
   , mgTransDeps
   , showModMsg
   , moduleGraphNodeModule
   , moduleGraphNodeModSum
   , moduleGraphModulesBelow

   , moduleGraphNodes
   , moduleGraphNodesZero
   , SummaryNode
   , summaryNodeSummary

   , NodeKey(..)
   , nodeKeyUnitId
   , nodeKeyModName
   , nodeKeyLevel
   , ModNodeKey
   , mkNodeKey
   , msKey

   , mkTransDepsZero



   , moduleGraphNodeUnitId

    , ModNodeKeyWithUid(..)

   , ModuleStage(..)
   , zeroStage
   , todoStage
   , moduleStageToThLevel
   , incModuleStage
   , decModuleStage
   , collapseModuleGraph
   , collapseModuleGraphNodes
   )
where

import GHC.Prelude
import GHC.Platform

import qualified GHC.LanguageExtensions as LangExt

import GHC.Data.Maybe
import GHC.Data.Graph.Directed

import GHC.Driver.Backend
import GHC.Driver.DynFlags

import GHC.Types.SourceFile ( hscSourceString )

import GHC.Unit.Module.ModSummary
import GHC.Unit.Types
import GHC.Utils.Outputable
import GHC.Utils.Misc ( partitionWith )

import System.FilePath
import qualified Data.Map as Map
import GHC.Types.Unique.DSet
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Unit.Module
import GHC.Linker.Static.Utils

import Data.Bifunctor
import Data.Function
import Data.List (sort, nub)
import GHC.Data.List.SetOps
import GHC.Stack
import GHC.Utils.Panic
import GHC.Unit.State
import Language.Haskell.Syntax.ImpExp

-- | A '@ModuleGraphNode@' is a node in the '@ModuleGraph@'.
-- Edges between nodes mark dependencies arising from module imports
-- and dependencies arising from backpack instantiations.
data ModuleGraphNode
  -- | Instantiation nodes track the instantiation of other units
  -- (backpack dependencies) with the holes (signatures) of the current package.
  = InstantiationNode UnitId InstantiatedUnit
  -- | There is a module summary node for each module, signature, and boot module being built.
  | ModuleNode [NodeKey] [(ModuleStage, UnitId)] ModuleStage ModSummary
  -- | Link nodes are whether are are creating a linked product (ie executable/shared object etc) for a unit.
  | LinkNode [NodeKey] UnitId

moduleGraphNodeModule :: ModuleGraphNode -> Maybe ModuleName
moduleGraphNodeModule mgn = ms_mod_name <$> (moduleGraphNodeModSum mgn)

moduleGraphNodeModSum :: ModuleGraphNode -> Maybe ModSummary
moduleGraphNodeModSum (InstantiationNode {}) = Nothing
moduleGraphNodeModSum (LinkNode {})          = Nothing
moduleGraphNodeModSum (ModuleNode _ _ _ ms)      = Just ms

moduleGraphNodeUnitId :: ModuleGraphNode -> UnitId
moduleGraphNodeUnitId mgn =
  case mgn of
    InstantiationNode uid _iud -> uid
    ModuleNode _ _lvl _ ms       -> toUnitId (moduleUnit (ms_mod ms))
    LinkNode _ uid             -> uid

instance Outputable ModuleGraphNode where
  ppr = \case
    InstantiationNode _ iuid -> ppr iuid
    ModuleNode nks uids lvl ms -> ppr (msKey lvl ms) <+> ppr nks <+> ppr uids
    LinkNode uid _     -> text "LN:" <+> ppr uid

instance Eq ModuleGraphNode where
  (==) = (==) `on` mkNodeKey

instance Ord ModuleGraphNode where
  compare = compare `on` mkNodeKey

data NodeKey = NodeKey_Unit {-# UNPACK #-} !InstantiatedUnit
             | NodeKey_Module {-# UNPACK #-} !ModNodeKeyWithUid
             | NodeKey_Link !UnitId
  deriving (Eq, Ord)

instance Outputable NodeKey where
  ppr nk = pprNodeKey nk

pprNodeKey :: NodeKey -> SDoc
pprNodeKey (NodeKey_Unit iu) = ppr iu
pprNodeKey (NodeKey_Module mk) = ppr mk
pprNodeKey (NodeKey_Link uid)  = ppr uid

nodeKeyUnitId :: NodeKey -> UnitId
nodeKeyUnitId (NodeKey_Unit iu)   = instUnitInstanceOf iu
nodeKeyUnitId (NodeKey_Module mk) = mnkUnitId mk
nodeKeyUnitId (NodeKey_Link uid)  = uid

nodeKeyLevel :: NodeKey -> ModuleStage
nodeKeyLevel (NodeKey_Unit {}) = zeroStage
nodeKeyLevel (NodeKey_Module mk) = mnkLevel mk
nodeKeyLevel (NodeKey_Link {}) = zeroStage

nodeKeyModName :: NodeKey -> Maybe ModuleName
nodeKeyModName (NodeKey_Module mk) = Just (gwib_mod $ mnkModuleName mk)
nodeKeyModName _ = Nothing

newtype ModuleStage = ModuleStage Int deriving (Eq, Ord)

instance Outputable ModuleStage where
  ppr (ModuleStage p) = ppr p

zeroStage :: ModuleStage
zeroStage = ModuleStage 1

todoStage :: HasCallStack => ModuleStage
todoStage -- = pprTrace "todoStage" callStackDoc
          = zeroStage

moduleStageToThLevel :: ModuleStage -> Int
moduleStageToThLevel (ModuleStage m) = m

decModuleStage, incModuleStage :: ModuleStage -> ModuleStage
incModuleStage (ModuleStage m) = ModuleStage (m + 1)
decModuleStage (ModuleStage m) = ModuleStage (m - 1)

data ModNodeKeyWithUid = ModNodeKeyWithUid { mnkModuleName :: !ModuleNameWithIsBoot
                                           , mnkLevel      :: !ModuleStage
                                           , mnkUnitId     :: !UnitId } deriving (Eq, Ord)

instance Outputable ModNodeKeyWithUid where
  ppr (ModNodeKeyWithUid mnwib lvl uid)
    | lvl == zeroStage = ppr uid <> colon <> ppr mnwib
    | otherwise = ppr uid <> colon <> ppr mnwib <> text "@" <> ppr lvl

-- | A '@ModuleGraph@' contains all the nodes from the home package (only). See
-- '@ModuleGraphNode@' for information about the nodes.
--
-- Modules need to be compiled. hs-boots need to be typechecked before
-- the associated "real" module so modules with {-# SOURCE #-} imports can be
-- built. Instantiations also need to be typechecked to ensure that the module
-- fits the signature. Substantiation typechecking is roughly comparable to the
-- check that the module and its hs-boot agree.
--
-- The graph is not necessarily stored in topologically-sorted order.  Use
-- 'GHC.topSortModuleGraph' and 'GHC.Data.Graph.Directed.flattenSCC' to achieve this.
data ModuleGraph = ModuleGraph
  { mg_mss :: [ModuleGraphNode]
  , mg_trans_deps :: Map.Map NodeKey (Set.Set NodeKey)
    -- A cached transitive dependency calculation so that a lot of work is not
    -- repeated whenever the transitive dependencies need to be calculated (for example, hptInstances)
  }

-- | Map a function 'f' over all the 'ModSummaries'.
-- To preserve invariants 'f' can't change the isBoot status.
mapMG :: (ModSummary -> ModSummary) -> ModuleGraph -> ModuleGraph
mapMG f mg@ModuleGraph{..} = mg
  { mg_mss = flip fmap mg_mss $ \case
      InstantiationNode uid iuid -> InstantiationNode uid iuid
      LinkNode uid nks -> LinkNode uid nks
      ModuleNode deps uid lvl ms  -> ModuleNode deps uid lvl (f ms)
  }

unionMG :: ModuleGraph -> ModuleGraph -> ModuleGraph
unionMG a b =
  let new_mss = nubOrdBy compare $ mg_mss a `mappend` mg_mss b
  in ModuleGraph {
        mg_mss = new_mss
      , mg_trans_deps = mkTransDeps new_mss
      }


mgTransDeps :: ModuleGraph -> Map.Map NodeKey (Set.Set NodeKey)
mgTransDeps = mg_trans_deps

mgModSummaries :: ModuleGraph -> [ModSummary]
mgModSummaries mg = [ m | ModuleNode _ _ _lvl m <- mgModSummaries' mg ]

mgModSummaries' :: ModuleGraph -> [ModuleGraphNode]
mgModSummaries' = mg_mss

-- | Look up a ModSummary in the ModuleGraph
-- Looks up the non-boot ModSummary
-- Linear in the size of the module graph
-- MP: This should probably be level aware
mgLookupModule :: ModuleGraph -> Module -> Maybe ModSummary
mgLookupModule ModuleGraph{..} m = listToMaybe $ mapMaybe go mg_mss
  where
    go (ModuleNode _ _ _lvl ms)
      | NotBoot <- isBootSummary ms
      , ms_mod ms == m
      = Just ms
    go _ = Nothing

emptyMG :: ModuleGraph
emptyMG = ModuleGraph [] Map.empty

isTemplateHaskellOrQQNonBoot :: ModSummary -> Bool
isTemplateHaskellOrQQNonBoot ms =
  (xopt LangExt.TemplateHaskell (ms_hspp_opts ms)
    || xopt LangExt.QuasiQuotes (ms_hspp_opts ms)) &&
  (isBootSummary ms == NotBoot)

isExplicitStageMS :: ModSummary -> Bool
isExplicitStageMS ms = xopt LangExt.StagedImports (ms_hspp_opts ms)

-- | Add an ExtendedModSummary to ModuleGraph. Assumes that the new ModSummary is
-- not an element of the ModuleGraph.
extendMG :: ModuleGraph -> [NodeKey] -> [(ModuleStage, UnitId)] -> ModuleStage -> ModSummary -> ModuleGraph
extendMG ModuleGraph{..} deps uid lvl ms = ModuleGraph
  { mg_mss = ModuleNode deps uid lvl ms : mg_mss
  , mg_trans_deps = mkTransDeps (ModuleNode deps uid lvl ms : mg_mss)
  }

mkTransDeps :: [ModuleGraphNode] -> Map.Map NodeKey (Set.Set NodeKey)
mkTransDeps mss =
  let (gg, _lookup_node) = moduleGraphNodes False mss
  in allReachable gg (mkNodeKey . node_payload)

mkTransDepsZero :: UnitState -> [ModuleGraphNode] -> Map.Map (Either (ModNodeKeyWithUid, ImportStage) UnitId) (Set.Set (Either (ModNodeKeyWithUid, ImportStage) UnitId))
mkTransDepsZero us mss =
  let (gg, _lookup_node) = moduleGraphNodesZero us mss
  in allReachable gg node_payload

extendMGInst :: ModuleGraph -> UnitId -> InstantiatedUnit -> ModuleGraph
extendMGInst mg uid depUnitId = mg
  { mg_mss = InstantiationNode uid depUnitId : mg_mss mg
  }

extendMGLink :: ModuleGraph -> UnitId -> [NodeKey] -> ModuleGraph
extendMGLink mg uid nks = mg { mg_mss = LinkNode nks uid : mg_mss mg }

extendMG' :: ModuleGraph -> ModuleGraphNode -> ModuleGraph
extendMG' mg = \case
  InstantiationNode uid depUnitId -> extendMGInst mg uid depUnitId
  ModuleNode deps uid lvl ms -> extendMG mg deps uid lvl ms
  LinkNode deps uid   -> extendMGLink mg uid deps

mkModuleGraph :: [ModuleGraphNode] -> ModuleGraph
mkModuleGraph = foldr (flip extendMG') emptyMG

collapseModuleGraph :: ModuleGraph -> ModuleGraph
collapseModuleGraph = mkModuleGraph . collapseModuleGraphNodes . mgModSummaries'

-- Collapse information about levels and map everything to level 0
collapseModuleGraphNodes :: [ModuleGraphNode] -> [ModuleGraphNode]
collapseModuleGraphNodes m = nub $ map go m
  where
    go (ModuleNode deps uid _lvl ms) = ModuleNode (nub $ map collapseNodeKey deps) uid zeroStage ms
    go (LinkNode deps uid) = LinkNode (nub $ map collapseNodeKey deps) uid
    go (InstantiationNode uid iuid) = InstantiationNode uid iuid

collapseNodeKey :: NodeKey -> NodeKey
collapseNodeKey (NodeKey_Module (ModNodeKeyWithUid mn _lvl uid))
  = NodeKey_Module (ModNodeKeyWithUid mn zeroStage uid)
collapseNodeKey n = n


-- | This function filters out all the instantiation nodes from each SCC of a
-- topological sort. Use this with care, as the resulting "strongly connected components"
-- may not really be strongly connected in a direct way, as instantiations have been
-- removed. It would probably be best to eliminate uses of this function where possible.
--
-- Nor account for levels
filterToposortToModules
  :: [SCC ModuleGraphNode] -> [SCC ModSummary]
filterToposortToModules = mapMaybe $ mapMaybeSCC $ \case
  InstantiationNode _ _ -> Nothing
  LinkNode{} -> Nothing
  ModuleNode _deps _uid _lvl node -> Just node
  where
    -- This higher order function is somewhat bogus,
    -- as the definition of "strongly connected component"
    -- is not necessarily respected.
    mapMaybeSCC :: (a -> Maybe b) -> SCC a -> Maybe (SCC b)
    mapMaybeSCC f = \case
      AcyclicSCC a -> AcyclicSCC <$> f a
      CyclicSCC as -> case mapMaybe f as of
        [] -> Nothing
        [a] -> Just $ AcyclicSCC a
        as -> Just $ CyclicSCC as

showModMsg :: DynFlags -> Bool -> ModuleGraphNode -> SDoc
showModMsg dflags _ (LinkNode {}) =
      let staticLink = case ghcLink dflags of
                          LinkStaticLib -> True
                          _ -> False

          platform  = targetPlatform dflags
          arch_os   = platformArchOS platform
          exe_file  = exeFileName arch_os staticLink (outputFile_ dflags)
      in text exe_file
showModMsg _ _ (InstantiationNode _uid indef_unit) =
  ppr $ instUnitInstanceOf indef_unit
showModMsg dflags recomp (ModuleNode _ _  lvl mod_summary) =
  if gopt Opt_HideSourcePaths dflags
      then text mod_str
      else hsep $
         [ text (mod_str ++ replicate (max 0 (16 - length mod_str)) ' ')
         , (if lvl == zeroStage then empty else ppr lvl)
         , char '('
         , text (op $ msHsFilePath mod_summary) <> char ','
         , message, char ')' ]

  where
    op       = normalise
    mod_str  = moduleNameString (moduleName (ms_mod mod_summary)) ++
               hscSourceString (ms_hsc_src mod_summary)
    dyn_file = op $ msDynObjFilePath mod_summary
    obj_file = op $ msObjFilePath mod_summary
    files    = [ obj_file ]
               ++ [ dyn_file | gopt Opt_BuildDynamicToo dflags ]
               ++ [ "interpreted" | gopt Opt_ByteCodeAndObjectCode dflags ]
    message = case backendSpecialModuleSource (backend dflags) recomp of
                Just special -> text special
                Nothing -> foldr1 (\ofile rest -> ofile <> comma <+> rest) (map text files)



type SummaryNode = Node Int ModuleGraphNode

summaryNodeKey :: SummaryNode -> Int
summaryNodeKey = node_key

summaryNodeSummary :: SummaryNode -> ModuleGraphNode
summaryNodeSummary = node_payload

-- | Collect the immediate dependencies of a ModuleGraphNode,
-- optionally avoiding hs-boot dependencies.
-- If the drop_hs_boot_nodes flag is False, and if this is a .hs and there is
-- an equivalent .hs-boot, add a link from the former to the latter.  This
-- has the effect of detecting bogus cases where the .hs-boot depends on the
-- .hs, by introducing a cycle.  Additionally, it ensures that we will always
-- process the .hs-boot before the .hs, and so the HomePackageTable will always
-- have the most up to date information.
nodeDependencies :: Bool -> ModuleGraphNode -> [NodeKey]
nodeDependencies drop_hs_boot_nodes = \case
    LinkNode deps _uid -> deps
    InstantiationNode uid iuid ->
      NodeKey_Module . (\mod -> ModNodeKeyWithUid (GWIB mod NotBoot) zeroStage uid)  <$> uniqDSetToList (instUnitHoles iuid)
    ModuleNode deps _ _lvl _ms ->
      map drop_hs_boot deps
  where
    -- Drop hs-boot nodes by using HsSrcFile as the key
    hs_boot_key | drop_hs_boot_nodes = NotBoot -- is regular mod or signature
                | otherwise          = IsBoot

    drop_hs_boot (NodeKey_Module (ModNodeKeyWithUid (GWIB mn IsBoot) lvl uid)) = (NodeKey_Module (ModNodeKeyWithUid (GWIB mn hs_boot_key) lvl uid))
    drop_hs_boot x = x

-- | Turn a list of graph nodes into an efficient queriable graph.
-- The first boolean parameter indicates whether nodes corresponding to hs-boot files
-- should be collapsed into their relevant hs nodes.
moduleGraphNodes :: Bool
  -> [ModuleGraphNode]
  -> (Graph SummaryNode, NodeKey -> Maybe SummaryNode)
moduleGraphNodes drop_hs_boot_nodes summaries =
  (graphFromEdgedVerticesUniq nodes, lookup_node)
  where
    -- Map from module to extra boot summary dependencies which need to be merged in
    (boot_summaries, nodes) = bimap Map.fromList id $ partitionWith go numbered_summaries

      where
        go (s, key) =
          case s of
                ModuleNode __deps _uid _lvl ms | isBootSummary ms == IsBoot, drop_hs_boot_nodes
                  -- Using nodeDependencies here converts dependencies on other
                  -- boot files to dependencies on dependencies on non-boot files.
                  -> Left (ms_mod ms, nodeDependencies drop_hs_boot_nodes s)
                _ -> normal_case
          where
           normal_case =
              let lkup_key = ms_mod <$> moduleGraphNodeModSum s
                  extra = (lkup_key >>= \key -> Map.lookup key boot_summaries)

              in Right $ DigraphNode s key $ out_edge_keys $
                      (fromMaybe [] extra
                        ++ nodeDependencies drop_hs_boot_nodes s)

    numbered_summaries = zip summaries [1..]

    lookup_node :: NodeKey -> Maybe SummaryNode
    lookup_node key = Map.lookup key (unNodeMap node_map)

    lookup_key :: NodeKey -> Maybe Int
    lookup_key = fmap summaryNodeKey . lookup_node

    node_map :: NodeMap SummaryNode
    node_map = NodeMap $
      Map.fromList [ (mkNodeKey s, node)
                   | node <- nodes
                   , let s = summaryNodeSummary node
                   ]

    out_edge_keys :: [NodeKey] -> [Int]
    out_edge_keys = mapMaybe lookup_key
        -- If we want keep_hi_boot_nodes, then we do lookup_key with
        -- IsBoot; else False


type ZeroSummaryNode = Node Int (Either (ModNodeKeyWithUid, ImportStage) UnitId)

zeroSummaryNodeKey :: ZeroSummaryNode -> Int
zeroSummaryNodeKey = node_key

zeroSummaryNodeSummary :: ZeroSummaryNode -> Either (ModNodeKeyWithUid, ImportStage) UnitId
zeroSummaryNodeSummary = node_payload

-- | Turn a list of graph nodes into an efficient queriable graph.
-- The first boolean parameter indicates whether nodes corresponding to hs-boot files
-- should be collapsed into their relevant hs nodes.
--
-- This graph only has edges between level-0 imports
--
--
-- This query answers the question. If I am looking at level n in module M then which
-- modules are visible?
--
-- If you are looking at level -1  then the reachable modules are those imported at splice and
-- then any modules those modules import at zero. (Ie the zero scope for those modules)
moduleGraphNodesZero ::
  UnitState
  -> [ModuleGraphNode]
  -> (Graph ZeroSummaryNode, Either (ModNodeKeyWithUid, ImportStage) UnitId -> Maybe ZeroSummaryNode)
moduleGraphNodesZero us summaries =
  (graphFromEdgedVerticesUniq nodes, lookup_node)
  where
    -- Map from module to extra boot summary dependencies which need to be merged in
    (nodes) = mapMaybe go numbered_summaries

      where
        go :: ((Either (ModuleGraphNode, ImportStage) (UnitId, [UnitId])), Int) -> Maybe ZeroSummaryNode
        go (s, key) = normal_case s
          where
           normal_case :: Either (ModuleGraphNode, ImportStage) (UnitId, [UnitId]) -> Maybe ZeroSummaryNode
           normal_case (Left ((ModuleNode nks uids lvl ms), s)) = Just $
                  DigraphNode (Left (msKey lvl ms, s)) key $ out_edge_keys (jimmy_lvl lvl s) $
                       ((map Left $ only_module_deps nks)
                        ++ (map Right uids))
           normal_case (Right (u, us)) =
             Just $ DigraphNode (Right u) key (mapMaybe lookup_key $ map Right us)
           normal_case _ = Nothing

    only_module_deps ds = [ k | NodeKey_Module k <- ds ]

    jimmy_lvl l s = case s of
                      NormalStage -> l
                      QuoteStage -> incModuleStage l
                      SpliceStage -> decModuleStage l

    numbered_summaries :: [(Either (ModuleGraphNode, ImportStage) (UnitId, [UnitId]), Int)]
    numbered_summaries = zip (([Left (s, l) | s <- summaries, l <- [SpliceStage, QuoteStage, NormalStage]]) ++ map Right (Map.toList all_unit_depends)) [1..]

    all_unit_depends :: Map.Map UnitId [UnitId]
    all_unit_depends = foldr (\m cache -> go cache (unit_depends m)) Map.empty summaries
      where

        go cache [] = cache
        go cache (u:uxs) =
          case Map.lookup u cache of
            Just {} -> go cache uxs
            Nothing -> case unitDepends <$> lookupUnitId us u of
                          Just us -> go (go (Map.insert u us cache) us) uxs
                          Nothing -> panic "bad"


    unit_depends :: ModuleGraphNode -> [UnitId]
    unit_depends (ModuleNode _ uids _ _) = map snd $ filter ((== zeroStage) . fst) uids
    unit_depends _ = []

    lookup_node :: Either (ModNodeKeyWithUid, ImportStage) UnitId -> Maybe ZeroSummaryNode
    lookup_node key = Map.lookup key node_map

    lookup_key :: Either (ModNodeKeyWithUid, ImportStage) UnitId -> Maybe Int
    lookup_key = fmap zeroSummaryNodeKey . lookup_node

    node_map :: Map.Map (Either (ModNodeKeyWithUid, ImportStage) UnitId) ZeroSummaryNode
    node_map =
      Map.fromList [ (s, node)
                   | node <- nodes
                   , let s = zeroSummaryNodeSummary node
                   ]

    out_edge_keys :: ModuleStage -> [Either ModNodeKeyWithUid (ModuleStage, UnitId)] -> [Int]
    out_edge_keys m = mapMaybe lookup_key . map (bimap (, NormalStage) snd) . filter (either (\nk -> mnkLevel nk == m) ((== m) . fst))
        -- If we want keep_hi_boot_nodes, then we do lookup_key with
        -- IsBoot; else False

newtype NodeMap a = NodeMap { unNodeMap :: Map.Map NodeKey a }
  deriving (Functor, Traversable, Foldable)

mkNodeKey :: ModuleGraphNode -> NodeKey
mkNodeKey = \case
  InstantiationNode _ iu -> NodeKey_Unit iu
  ModuleNode _ _ lvl x -> NodeKey_Module $ msKey lvl x
  LinkNode _ uid   -> NodeKey_Link uid

msKey :: ModuleStage -> ModSummary -> ModNodeKeyWithUid
msKey l ms = ModNodeKeyWithUid (ms_mnwib ms) l (ms_unitid ms)

type ModNodeKey = ModuleNameWithIsBoot


-- | This function returns all the modules belonging to the home-unit that can
-- be reached by following the given dependencies. Additionally, if both the
-- boot module and the non-boot module can be reached, it only returns the
-- non-boot one.
moduleGraphModulesBelow :: ModuleGraph -> UnitId -> ModuleStage -> ModuleNameWithIsBoot -> Set ModNodeKeyWithUid
moduleGraphModulesBelow mg uid lvl mn = filtered_mods $ [ mn |  NodeKey_Module mn <- modules_below]
  where
    td_map = mgTransDeps mg

    modules_below = maybe [] Set.toList $ Map.lookup (NodeKey_Module (ModNodeKeyWithUid mn lvl uid)) td_map

    filtered_mods = Set.fromDistinctAscList . filter_mods . sort

    -- IsBoot and NotBoot modules are necessarily consecutive in the sorted list
    -- (cf Ord instance of GenWithIsBoot). Hence we only have to perform a
    -- linear sweep with a window of size 2 to remove boot modules for which we
    -- have the corresponding non-boot.
    filter_mods = \case
      (r1@(ModNodeKeyWithUid (GWIB m1 b1) lvl1 uid1) : r2@(ModNodeKeyWithUid (GWIB m2 _) lvl2 uid2): rs)
        | m1 == m2  && uid1 == uid2 && lvl1 == lvl2 ->
                       let !r' = case b1 of
                                  NotBoot -> r1
                                  IsBoot  -> r2
                       in r' : filter_mods rs
        | otherwise -> r1 : filter_mods (r2:rs)
      rs -> rs

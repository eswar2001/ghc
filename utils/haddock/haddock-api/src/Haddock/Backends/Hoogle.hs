{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Haddock.Backends.Hoogle
-- Copyright   :  (c) Neil Mitchell 2006-2008
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Write out Hoogle compatible documentation
-- http://www.haskell.org/hoogle/
module Haddock.Backends.Hoogle
  ( -- * Main entry point to Hoogle output generation
    ppHoogle

    -- * Utilities for generating Hoogle output during interface creation
  , ppExportD
  ) where

import Data.Char
import Data.Foldable (toList)
import Data.List (intercalate, isPrefixOf)
import Data.Maybe
import Data.Version
import GHC
import GHC.Core.InstEnv
import qualified GHC.Driver.DynFlags as DynFlags
import GHC.Driver.Ppr
import GHC.Plugins (TopLevelFlag (..))
import GHC.Types.SourceText
import GHC.Unit.State
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic
import System.Directory
import System.FilePath

import Documentation.Haddock.Markup
import Haddock.GhcUtils
import Haddock.Types hiding (Version)
import Haddock.Utils hiding (out)

prefix :: [String]
prefix =
  [ "-- Hoogle documentation, generated by Haddock"
  , "-- See Hoogle, http://www.haskell.org/hoogle/"
  , ""
  ]

ppHoogle :: DynFlags -> UnitState -> String -> Version -> String -> Maybe (Doc RdrName) -> [Interface] -> FilePath -> IO ()
ppHoogle dflags unit_state package version synopsis prologue ifaces odir = do
  let
    -- Since Hoogle is line based, we want to avoid breaking long lines.
    dflags' = dflags{pprCols = maxBound}
    sDocContext = DynFlags.initSDocContext dflags' Outputable.defaultUserStyle
    filename = package ++ ".txt"
    contents =
      prefix
        ++ docWith sDocContext (drop 2 $ dropWhile (/= ':') synopsis) prologue
        ++ ["@package " ++ package]
        ++ [ "@version " ++ showVersion version
           | not (null (versionBranch version))
           ]
        ++ concat [ppModule dflags' sDocContext unit_state i | i <- ifaces, OptHide `notElem` ifaceOptions i]
  createDirectoryIfMissing True odir
  writeUtf8File (odir </> filename) (unlines contents)

ppModule :: DynFlags -> SDocContext -> UnitState -> Interface -> [String]
ppModule dflags sDocContext unit_state iface =
  ""
    : ppDocumentation sDocContext (ifaceDoc iface)
    ++ ["module " ++ moduleString (ifaceMod iface)]
    ++ concatMap ppExportItem (ifaceRnExportItems iface)
    ++ concatMap (ppInstance dflags unit_state) (ifaceInstances iface)

-- | If the export item is an 'ExportDecl', get the attached Hoogle textual
-- database entries for that export declaration.
ppExportItem :: ExportItem DocNameI -> [String]
ppExportItem (ExportDecl RnExportD{rnExpDHoogle = o}) = o
ppExportItem _ = []

---------------------------------------------------------------------
-- Utility functions

dropHsDocTy :: HsSigType GhcRn -> HsSigType GhcRn
dropHsDocTy = drop_sig_ty
  where
    drop_sig_ty (HsSig x a b) = HsSig x a (drop_lty b)
    drop_sig_ty x@XHsSigType{} = x

    drop_lty (L src x) = L src (drop_ty x)

    drop_ty (HsForAllTy x a e) = HsForAllTy x a (drop_lty e)
    drop_ty (HsQualTy x a e) = HsQualTy x a (drop_lty e)
    drop_ty (HsBangTy x a b) = HsBangTy x a (drop_lty b)
    drop_ty (HsAppTy x a b) = HsAppTy x (drop_lty a) (drop_lty b)
    drop_ty (HsAppKindTy x a b) = HsAppKindTy x (drop_lty a) (drop_lty b)
    drop_ty (HsFunTy x w a b) = HsFunTy x w (drop_lty a) (drop_lty b)
    drop_ty (HsListTy x a) = HsListTy x (drop_lty a)
    drop_ty (HsTupleTy x a b) = HsTupleTy x a (map drop_lty b)
    drop_ty (HsOpTy x p a b c) = HsOpTy x p (drop_lty a) b (drop_lty c)
    drop_ty (HsParTy x a) = HsParTy x (drop_lty a)
    drop_ty (HsKindSig x a b) = HsKindSig x (drop_lty a) b
    drop_ty (HsDocTy _ a _) = drop_ty $ unL a
    drop_ty x = x

outHsSigType :: SDocContext -> HsSigType GhcRn -> String
outHsSigType sDocContext = out sDocContext . reparenSigType . dropHsDocTy

dropComment :: String -> String
dropComment (' ' : '-' : '-' : ' ' : _) = []
dropComment (x : xs) = x : dropComment xs
dropComment [] = []

outWith :: Outputable a => (SDoc -> String) -> a -> [Char]
outWith p =
  f
    . unwords
    . map (dropWhile isSpace)
    . lines
    . p
    . ppr
  where
    f xs | " <document comment>" `isPrefixOf` xs = f $ drop 19 xs
    f (x : xs) = x : f xs
    f [] = []

out :: Outputable a => SDocContext -> a -> String
out sDocContext = outWith $ Outputable.renderWithContext sDocContext

operator :: String -> String
operator (x : xs) | not (isAlphaNum x) && x `notElem` "_' ([{" = '(' : x : xs ++ ")"
operator x = x

commaSeparate :: Outputable a => SDocContext -> [a] -> String
commaSeparate sDocContext = Outputable.renderWithContext sDocContext . interpp'SP

---------------------------------------------------------------------
-- How to print each export

ppExportD :: DynFlags -> ExportD GhcRn -> [String]
ppExportD
  dflags
  ExportD
    { expDDecl = L _ decl
    , expDPats = bundledPats
    , expDMbDoc = mbDoc
    , expDSubDocs = subdocs
    , expDFixities = fixities
    } =
    concat
      [ ppDocumentation sDocContext dc ++ f d
      | (d, (dc, _)) <- (decl, mbDoc) : bundledPats
      ]
      ++ ppFixities
    where
      f :: HsDecl GhcRn -> [String]
      f (TyClD _ d@DataDecl{}) = ppData sDocContext d subdocs
      f (TyClD _ d@SynDecl{}) = ppSynonym sDocContext d
      f (TyClD _ d@ClassDecl{}) = ppClass sDocContext d subdocs
      f (TyClD _ (FamDecl _ d)) = ppFam sDocContext d
      f (ForD _ (ForeignImport _ name typ _)) = [ppSig sDocContext [name] typ]
      f (ForD _ (ForeignExport _ name typ _)) = [ppSig sDocContext [name] typ]
      f (SigD _ sig) = ppSigWithDoc sDocContext sig []
      f _ = []

      ppFixities :: [String]
      ppFixities = concatMap (ppFixity sDocContext) fixities

      sDocContext = DynFlags.initSDocContext dflags Outputable.defaultUserStyle

ppSigWithDoc :: SDocContext -> Sig GhcRn -> [(Name, DocForDecl Name)] -> [String]
ppSigWithDoc sDocContext sig subdocs = case sig of
  TypeSig _ names t -> concatMap (mkDocSig "" (dropWildCards t)) names
  PatSynSig _ names t -> concatMap (mkDocSig "pattern " t) names
  _ -> []
  where
    mkDocSig leader typ n =
      mkSubdocN
        sDocContext
        n
        subdocs
        [leader ++ ppSig sDocContext [n] typ]

ppSig :: SDocContext -> [LocatedN Name] -> LHsSigType GhcRn -> String
ppSig sDocContext names (L _ typ) =
  operator prettyNames ++ " :: " ++ outHsSigType sDocContext typ
  where
    prettyNames = intercalate ", " $ map (out sDocContext) names

-- note: does not yet output documentation for class methods
ppClass :: SDocContext -> TyClDecl GhcRn -> [(Name, DocForDecl Name)] -> [String]
ppClass sDocContext decl@(ClassDecl{}) subdocs =
  (ppDecl ++ ppTyFams) : ppMethods
  where
    ppDecl :: String
    ppDecl =
      out
        sDocContext
        decl
          { tcdSigs = []
          , tcdATs = []
          , tcdATDefs = []
          , tcdMeths = emptyLHsBinds
          }

    ppMethods :: [String]
    ppMethods = concat . map (ppSig' . unLoc . add_ctxt) $ tcdSigs decl

    ppSig' = flip (ppSigWithDoc sDocContext) subdocs

    add_ctxt = addClassContext (tcdName decl) (tyClDeclTyVars decl)

    ppTyFams :: String
    ppTyFams
      | null $ tcdATs decl = ""
      | otherwise =
          (" " ++) . Outputable.renderWithContext sDocContext . whereWrapper $
            concat
              [ map pprTyFam (tcdATs decl)
              , map (pprTyFamInstDecl NotTopLevel . unLoc) (tcdATDefs decl)
              ]

    pprTyFam :: LFamilyDecl GhcRn -> SDoc
    pprTyFam (L _ at) =
      vcat' $
        map text $
          mkSubdocN
            sDocContext
            (fdLName at)
            subdocs
            -- Associated type families should not be printed as top-level
            -- (avoid printing the `family` keyword)
            (ppFam sDocContext at{fdTopLevel = NotTopLevel})

    whereWrapper elems =
      vcat'
        [ text "where" <+> lbrace
        , nest 4 . vcat . map (Outputable.<> semi) $ elems
        , rbrace
        ]
ppClass _ _non_cls_decl _ = []

ppFam :: SDocContext -> FamilyDecl GhcRn -> [String]
ppFam sDocContext decl@(FamilyDecl{fdInfo = info}) =
  [out sDocContext decl']
  where
    decl' = case info of
      -- We don't need to print out a closed type family's equations
      -- for Hoogle, so pretend it doesn't have any.
      ClosedTypeFamily{} -> decl{fdInfo = OpenTypeFamily}
      _ -> decl

ppInstance :: DynFlags -> UnitState -> ClsInst -> [String]
ppInstance dflags unit_state x =
  [dropComment $ outWith (showSDocForUser dflags unit_state alwaysQualify) cls]
  where
    -- As per #168, we don't want safety information about the class
    -- in Hoogle output. The easiest way to achieve this is to set the
    -- safety information to a state where the Outputable instance
    -- produces no output which means no overlap and unsafe (or [safe]
    -- is generated).
    cls =
      x
        { is_flag =
            OverlapFlag
              { overlapMode = NoOverlap NoSourceText
              , isSafeOverlap = False
              }
        }

ppSynonym :: SDocContext -> TyClDecl GhcRn -> [String]
ppSynonym sDocContext x = [out sDocContext x]

ppData :: SDocContext -> TyClDecl GhcRn -> [(Name, DocForDecl Name)] -> [String]
ppData sDocContext decl@DataDecl{tcdLName = name, tcdTyVars = tvs, tcdFixity = fixity, tcdDataDefn = defn} subdocs =
  out sDocContext (ppDataDefnHeader (pp_vanilla_decl_head name tvs fixity) defn)
    : concatMap (ppCtor sDocContext decl subdocs . unLoc) (dd_cons defn)
ppData _ _ _ = panic "ppData"

-- | for constructors, and named-fields...
lookupCon :: SDocContext -> [(Name, DocForDecl Name)] -> LocatedN Name -> [String]
lookupCon sDocContext subdocs (L _ name) = case lookup name subdocs of
  Just (d, _) -> ppDocumentation sDocContext d
  _ -> []

ppCtor :: SDocContext -> TyClDecl GhcRn -> [(Name, DocForDecl Name)] -> ConDecl GhcRn -> [String]
ppCtor sDocContext dat subdocs con@ConDeclH98{con_args = con_args'} =
  -- AZ:TODO get rid of the concatMap
  concatMap (lookupCon sDocContext subdocs) [con_name con] ++ f con_args'
  where
    f (PrefixCon _ args) = [typeSig name $ (map hsScaledThing args) ++ [resType]]
    f (InfixCon a1 a2) = f $ PrefixCon [] [a1, a2]
    f (RecCon (L _ recs)) =
      f (PrefixCon [] $ map (hsLinear . cd_fld_type . unLoc) recs)
        ++ concat
          [ (concatMap (lookupCon sDocContext subdocs . noLocA . unLoc . foLabel . unLoc) (cd_fld_names r))
            ++ [out sDocContext (map (foExt . unLoc) $ cd_fld_names r) `typeSig` [resType, cd_fld_type r]]
          | r <- map unLoc recs
          ]

    funs = foldr1 (\x y -> reL $ HsFunTy noExtField (HsUnrestrictedArrow noExtField) x y)
    apps = foldl1 (\x y -> reL $ HsAppTy noExtField x y)

    typeSig nm flds =
      operator nm
        ++ " :: "
        ++ outHsSigType sDocContext (unL $ mkEmptySigType $ funs flds)

    -- We print the constructors as comma-separated list. See GHC
    -- docs for con_names on why it is a list to begin with.
    name = commaSeparate sDocContext . toList $ unL <$> getConNames con

    tyVarArg (HsTvb { tvb_var = bvar, tvb_kind = bkind }) = tvk
      where
        tv, tvk :: HsType GhcRn
        tv = case bvar of
          HsBndrVar _ n -> HsTyVar noAnn NotPromoted n
          HsBndrWildCard _ -> HsWildCardTy noExtField
        tvk = case bkind of
          HsBndrNoKind _   -> tv
          HsBndrKind _ lty -> HsKindSig noAnn (reL tv) lty

    resType =
      apps $
        map reL $
          (HsTyVar noAnn NotPromoted (reL (tcdName dat)))
            : map (tyVarArg . unLoc) (hsQTvExplicit $ tyClDeclTyVars dat)
ppCtor
  sDocContext
  _dat
  subdocs
  ( ConDeclGADT
      { con_names = names
      , con_bndrs = L _ outer_bndrs
      , con_mb_cxt = mcxt
      , con_g_args = args
      , con_res_ty = res_ty
      }
    ) =
    concatMap (lookupCon sDocContext subdocs) names ++ [typeSig]
    where
      typeSig = operator name ++ " :: " ++ outHsSigType sDocContext con_sig_ty
      name = out sDocContext $ unL <$> names
      con_sig_ty = HsSig noExtField outer_bndrs theta_ty
        where
          theta_ty = case mcxt of
            Just theta -> noLocA (HsQualTy{hst_xqual = noExtField, hst_ctxt = theta, hst_body = tau_ty})
            Nothing -> tau_ty
          tau_ty = foldr mkFunTy res_ty $
            case args of
              PrefixConGADT _ pos_args -> map hsScaledThing pos_args
              RecConGADT _ (L _ flds) -> map (cd_fld_type . unL) flds
          mkFunTy a b = noLocA (HsFunTy noExtField (HsUnrestrictedArrow noExtField) a b)

ppFixity :: SDocContext -> (Name, Fixity) -> [String]
ppFixity sDocContext (name, fixity) = [out sDocContext ((FixitySig NoNamespaceSpecifier [noLocA name] fixity) :: FixitySig GhcRn)]

---------------------------------------------------------------------
-- DOCUMENTATION

ppDocumentation :: Outputable o => SDocContext -> Documentation o -> [String]
ppDocumentation sDocContext (Documentation d w) = mdoc sDocContext d ++ doc sDocContext w

doc :: Outputable o => SDocContext -> Maybe (Doc o) -> [String]
doc sDocContext = docWith sDocContext ""

mdoc :: Outputable o => SDocContext -> Maybe (MDoc o) -> [String]
mdoc sDocContext = docWith sDocContext "" . fmap _doc

docWith :: Outputable o => SDocContext -> String -> Maybe (Doc o) -> [String]
docWith _ [] Nothing = []
docWith sDocContext header d =
  ("" :) $
    zipWith (++) ("-- | " : repeat "--   ") $
      lines header
        ++ ["" | header /= "" && isJust d]
        ++ maybe [] (showTags . markup (markupTag sDocContext)) d

mkSubdocN :: SDocContext -> LocatedN Name -> [(Name, DocForDecl Name)] -> [String] -> [String]
mkSubdocN sDocContext n subdocs s = mkSubdoc sDocContext (la2la n) subdocs s

mkSubdoc :: SDocContext -> LocatedA Name -> [(Name, DocForDecl Name)] -> [String] -> [String]
mkSubdoc sDocContext n subdocs s = concatMap (ppDocumentation sDocContext) getDoc ++ s
  where
    getDoc = maybe [] (return . fst) (lookup (unLoc n) subdocs)

data Tag = TagL Char [Tags] | TagP Tags | TagPre Tags | TagInline String Tags | Str String
  deriving (Show)

type Tags = [Tag]

box :: (a -> b) -> a -> [b]
box f x = [f x]

str :: String -> [Tag]
str a = [Str a]

-- want things like paragraph, pre etc to be handled by blank lines in the source document
-- and things like \n and \t converted away
-- much like blogger in HTML mode
-- everything else wants to be included as tags, neatly nested for some (ul,li,ol)
-- or inlne for others (a,i,tt)
-- entities (&,>,<) should always be appropriately escaped

markupTag :: Outputable o => SDocContext -> DocMarkup o [Tag]
markupTag sDocContext =
  Markup
    { markupParagraph = box TagP
    , markupEmpty = str ""
    , markupString = str
    , markupAppend = (++)
    , markupIdentifier = box (TagInline "a") . str . out sDocContext
    , markupIdentifierUnchecked = box (TagInline "a") . str . showWrapped (out sDocContext . snd)
    , markupModule = \(ModLink m label) -> box (TagInline "a") (fromMaybe (str m) label)
    , markupWarning = box (TagInline "i")
    , markupEmphasis = box (TagInline "i")
    , markupBold = box (TagInline "b")
    , markupMonospaced = box (TagInline "tt")
    , markupPic = const $ str " "
    , markupMathInline = const $ str "<math>"
    , markupMathDisplay = const $ str "<math>"
    , markupUnorderedList = box (TagL 'u')
    , markupOrderedList = box (TagL 'o') . map snd
    , markupDefList = box (TagL 'u') . map (\(a, b) -> TagInline "i" a : Str " " : b)
    , markupCodeBlock = box TagPre
    , markupHyperlink = \(Hyperlink url mLabel) -> box (TagInline "a") (fromMaybe (str url) mLabel)
    , markupAName = const $ str ""
    , markupProperty = box TagPre . str
    , markupExample = box TagPre . str . unlines . map exampleToString
    , markupHeader = \(Header l h) -> box (TagInline $ "h" ++ show l) h
    , markupTable = \(Table _ _) -> str "TODO: table"
    }

showTags :: [Tag] -> [String]
showTags = intercalate [""] . map showBlock

showBlock :: Tag -> [String]
showBlock (TagP xs) = showInline xs
showBlock (TagL t xs) = ['<' : t : "l>"] ++ mid ++ ['<' : '/' : t : "l>"]
  where
    mid = concatMap (showInline . box (TagInline "li")) xs
showBlock (TagPre xs) = ["<pre>"] ++ showPre xs ++ ["</pre>"]
showBlock x = showInline [x]

asInline :: Tag -> Tags
asInline (TagP xs) = xs
asInline (TagPre xs) = [TagInline "pre" xs]
asInline (TagL t xs) = [TagInline (t : "l") $ map (TagInline "li") xs]
asInline x = [x]

showInline :: [Tag] -> [String]
showInline = unwordsWrap 70 . words . concatMap f
  where
    fs = concatMap f
    f (Str x) = escape x
    f (TagInline s xs) = "<" ++ s ++ ">" ++ (if s == "li" then trim else id) (fs xs) ++ "</" ++ s ++ ">"
    f x = fs $ asInline x

    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

showPre :: [Tag] -> [String]
showPre = trimFront . trimLines . lines . concatMap f
  where
    trimLines = dropWhile null . reverse . dropWhile null . reverse
    trimFront xs = map (drop i) xs
      where
        ns = [length a | x <- xs, let (a, b) = span isSpace x, b /= ""]
        i = if null ns then 0 else minimum ns

    fs = concatMap f
    f (Str x) = escape x
    f (TagInline s xs) = "<" ++ s ++ ">" ++ fs xs ++ "</" ++ s ++ ">"
    f x = fs $ asInline x

unwordsWrap :: Int -> [String] -> [String]
unwordsWrap n = f n []
  where
    f _ s [] = [g s | s /= []]
    f i s (x : xs)
      | nx > i = g s : f (n - nx - 1) [x] xs
      | otherwise = f (i - nx - 1) (x : s) xs
      where
        nx = length x

    g = unwords . reverse

escape :: String -> String
escape = concatMap f
  where
    f '<' = "&lt;"
    f '>' = "&gt;"
    f '&' = "&amp;"
    f x = [x]

-- | Just like 'vcat' but uses '($+$)' instead of '($$)'.
vcat' :: [SDoc] -> SDoc
vcat' = foldr ($+$) empty

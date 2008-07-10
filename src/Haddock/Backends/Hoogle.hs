--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--
-- This file, (c) Neil Mitchell 2006-2008
-- Write out Hoogle compatible documentation
-- http://www.haskell.org/hoogle/

module Haddock.Backends.Hoogle ( 
    ppHoogle
  ) where


import Haddock.GHC
import GHC hiding ((<.>))
import SrcLoc
import Outputable

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import System.FilePath


prefix = ["-- Hoogle documentation, generated by Haddock"
         ,"-- See Hoogle, http://www.haskell.org/hoogle/"
         ,""]


ppHoogle :: String -> String -> [Interface] -> FilePath -> IO ()
ppHoogle package version ifaces odir = do
    let filename = package <.> "txt"
        contents = prefix ++
                   ["@package " ++ package] ++
                   ["@version " ++ version | version /= ""] ++
                   concat [ppModule i | i <- ifaces, OptHide `notElem` ifaceOptions i]
    writeFile (odir </> filename) (unlines contents)


ppModule :: Interface -> [String]
ppModule iface = "" : doc (ifaceDoc iface) ++
                 ["module " ++ moduleString (ifaceMod iface)] ++
                 concatMap ppExport exported ++
                 concatMap ppInstance (ifaceInstances iface)
    where
        locals = Set.fromList $ ifaceLocals iface
        exported = [i | i@(ExportDecl{expItemDecl=decl}) <- ifaceExportItems iface
                      , isLocal (unLoc decl)]
        isLocal decl
          | Just name <- getMainDeclBinder decl = name `Set.member` locals
          | otherwise = False


---------------------------------------------------------------------
-- Utility functions

unL (L _ x) = x
reL = L undefined


dropComment (' ':'-':'-':' ':_) = []
dropComment (x:xs) = x : dropComment xs
dropComment [] = []


out :: Outputable a => a -> String
out = f . unwords . map (dropWhile isSpace) . lines . showSDocUnqual . ppr
    where
        f xs | " <document comment>" `isPrefixOf` xs = f $ drop 19 xs
        f (x:xs) = x : f xs
        f [] = []


typeSig :: String -> [String] -> String
typeSig name flds = operator name ++ " :: " ++ concat (intersperse " -> " flds)


operator :: String -> String
operator (x:xs) | not (isAlphaNum x) && x `notElem` " ([{" = "(" ++ x:xs ++ ")"
operator x = x


---------------------------------------------------------------------
-- How to print each export

ppExport :: ExportItem Name -> [String]
ppExport (ExportDecl decl dc _) = doc dc ++ f (unL decl)
    where
        f (TyClD d@TyData{}) = ppData d
        f (TyClD d@ClassDecl{}) = ppClass d
        f (ForD (ForeignImport name typ _)) = ppSig $ TypeSig name typ
        f (ForD (ForeignExport name typ _)) = ppSig $ TypeSig name typ
        f (SigD sig) = ppSig sig
        f _ = []
ppExport _ = []


ppSig :: Sig Name -> [String]
ppSig (TypeSig name sig) = [operator (out name) ++ " :: " ++ out sig]
ppSig _ = []


-- note: does not yet output documentation for class methods
ppClass :: TyClDecl Name -> [String]
ppClass x = out x{tcdSigs=[]} :
            map (out . addContext . unL) (tcdSigs x)
    where
        addContext (TypeSig name (L l sig)) = TypeSig name (L l $ f sig)
        f (HsForAllTy a b con d) = HsForAllTy a b (reL $ context : unL con) d
        f x = HsForAllTy Implicit [] (reL [context]) (reL x)

        context = reL $ HsClassP (unL $ tcdLName x)
            (map (reL . HsTyVar . tyVar . unL) (tcdTyVars x))

        tyVar (UserTyVar x) = x
        tyVar (KindedTyVar x _) = x


ppInstance :: Instance -> [String]
ppInstance x = [dropComment $ out x]


ppData :: TyClDecl Name -> [String]
ppData x = showData x{tcdCons=[],tcdDerivs=Nothing} :
           concatMap (ppCtor x . unL) (tcdCons x)
    where
        -- GHC gives out "data Bar =", we want to delete the equals
        -- also writes data : a b, when we want data (:) a b
        showData x = unwords $ map f $ if last xs == "=" then init xs else xs
            where
                xs = words $ out x
                nam = out $ tcdLName x
                f x = if x == nam then operator nam else x


ppCtor :: TyClDecl Name -> ConDecl Name -> [String]
ppCtor dat con = ldoc (con_doc con) ++ f (con_details con)
    where
        f (PrefixCon args) = [typeSig name $ map out args ++ [resType]]
        f (InfixCon a1 a2) = f $ PrefixCon [a1,a2]
        f (RecCon recs) = f (PrefixCon $ map cd_fld_type recs) ++ concat
                          [ldoc (cd_fld_doc r) ++
                           [out (unL $ cd_fld_name r) `typeSig` [resType, out $ cd_fld_type r]]
                          | r <- recs]

        name = out $ unL $ con_name con

        resType = case con_res con of
            ResTyH98 -> unwords $ operator (out (tcdLName dat)) : map out (tcdTyVars dat)
            ResTyGADT x -> out $ unL x


---------------------------------------------------------------------
-- DOCUMENTATION

ldoc :: Maybe (LHsDoc Name) -> [String]
ldoc = doc . liftM unL

doc :: Maybe (HsDoc Name) -> [String]
doc Nothing = []
doc (Just d) = "" : zipWith (++) ("-- | " : repeat "--   ") (showTags $ markup markupTag d)


data Tag = TagL Char [Tags] | TagP Tags | TagPre Tags | TagInline String Tags | Str String
           deriving Show

type Tags = [Tag]

box f x = [f x]
str a = [Str a]

-- want things like paragraph, pre etc to be handled by blank lines in the source document
-- and things like \n and \t converted away
-- much like blogger in HTML mode
-- everything else wants to be included as tags, neatly nested for some (ul,li,ol)
-- or inlne for others (a,i,tt)
-- entities (&,>,<) should always be appropriately escaped

markupTag :: DocMarkup Name [Tag]
markupTag = Markup {
  markupParagraph     = box TagP,
  markupEmpty         = str "",
  markupString        = str,
  markupAppend        = (++),
  markupIdentifier    = box (TagInline "a") . str . out . head,
  markupModule        = box (TagInline "a") . str,
  markupEmphasis      = box (TagInline "i"),
  markupMonospaced    = box (TagInline "tt"),
  markupUnorderedList = box (TagL 'u'),
  markupOrderedList   = box (TagL 'o'),
  markupDefList       = box (TagL 'u') . map (\(a,b) -> TagInline "b" a : Str " " : b),
  markupCodeBlock     = box TagPre,
  markupURL           = box (TagInline "a") . str,
  markupAName         = const $ str ""
  }


showTags :: [Tag] -> [String]
showTags = concat . intersperse [""] . map showBlock
    where


showBlock :: Tag -> [String]
showBlock (TagP xs) = showInline xs
showBlock (TagL t xs) = ['<':t:"l>"] ++ mid ++ ['<':'/':t:"l>"]
    where mid = concatMap (showInline . box (TagInline "li")) xs
showBlock (TagPre xs) = ["<pre>"] ++ showPre xs ++ ["</pre>"]
showBlock x = showInline [x]


asInline (TagP xs) = xs
asInline (TagPre xs) = [TagInline "pre" xs]
asInline (TagL t xs) = [TagInline (t:"l") $ map (TagInline "li") xs]
asInline x = [x]


showInline :: [Tag] -> [String]
showInline = unwordsWrap 70 . words . concatMap f
    where
        fs = concatMap f
        f (Str x) = escape x
        f (TagInline s xs) = "<"++s++">" ++ (if s == "li" then trim else id) (fs xs) ++ "</"++s++">"
        f x = fs $ asInline x

        trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse


showPre :: [Tag] -> [String]
showPre = trimFront . trimLines . lines . concatMap f
    where
        trimLines = dropWhile null . reverse . dropWhile null . reverse
        trimFront xs = map (drop i) xs
            where
                ns = [length a | x <- xs, let (a,b) = span isSpace x, b /= ""]
                i = if null ns then 0 else minimum ns

        fs = concatMap f
        f (Str x) = escape x
        f (TagInline s xs) = "<"++s++">" ++ fs xs ++ "</"++s++">"
        f x = fs $ asInline x


unwordsWrap :: Int -> [String] -> [String]
unwordsWrap n = f n []
    where
        f i s [] = [g s | s /= []]
        f i s (x:xs) | nx > i = g s : f (n - nx - 1) [x] xs
                     | otherwise = f (i - nx - 1) (x:s) xs
            where nx = length x

        g = unwords . reverse


escape :: String -> String
escape = concatMap f
    where
        f '<' = "&lt;"
        f '>' = "&gt;"
        f '&' = "&amp;"
        f x = [x]

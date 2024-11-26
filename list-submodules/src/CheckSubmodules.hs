{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module CheckSubmodules
    ( main
    ) where

import System.FilePath
import qualified Data.ByteString as BS
import Distribution.Types.PackageName (PackageName)
import qualified Distribution.PackageDescription.Parsec as C
import qualified Distribution.Types.Version as C
import qualified Distribution.Types.PackageId as C
import qualified Distribution.Types.GenericPackageDescription as C
import qualified Distribution.Types.PackageDescription as C
import Control.Monad.Trans.Writer
import Data.Version

import Hackage (getVersions, PackageState (..))
import qualified Distribution.Types.PackageName as C
import Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import System.Exit

isPvpCompatible :: Version -> Version -> Bool
isPvpCompatible a b =
    take 2 (versionBranch a) == take 2 (versionBranch b)

data Package = Package { pkgName :: PackageName
                       , pkgPath :: FilePath
                       }

packages :: [Package]
packages =
    [ Package "file-io" "libraries/file-io"
    , Package "hsc2hs" "utils/hsc2hs"
    , Package "Cabal" "libraries/Cabal/Cabal"
    , Package "Cabal-syntax" "libraries/Cabal/Cabal-syntax"
    , Package "bytestring" "libraries/bytestring"
    , Package "binary" "libraries/binary"
    , Package "array" "libraries/array"
    , Package "containers" "libraries/containers/containers"
    , Package "deepseq" "libraries/deepseq"
    , Package "directory" "libraries/directory"
    , Package "filepath" "libraries/filepath"
    , Package "haskeline" "libraries/haskeline"
    , Package "hpc" "libraries/hpc"
    , Package "mtl" "libraries/mtl"
    , Package "parsec" "libraries/parsec"
    , Package "pretty" "libraries/pretty"
    , Package "process" "libraries/process"
    , Package "terminfo" "libraries/terminfo"
    , Package "text" "libraries/text"
    , Package "time" "libraries/time"
    , Package "unix" "libraries/unix"
    , Package "exceptions" "libraries/exceptions"
    , Package "semaphore-compat" "libraries/semaphore-compat"
    , Package "stm" "libraries/stm"
    , Package "Win32" "libraries/Win32"
    , Package "xhtml" "libraries/xhtml"
    ]

getPackageVersion :: Package -> IO Version
getPackageVersion pkg = do
    Just gpd <- C.parseGenericPackageDescriptionMaybe <$> BS.readFile (pkgPath pkg </> C.unPackageName (pkgName pkg) <.> "cabal")
    let v = C.pkgVersion $ C.package $ C.packageDescription gpd
    return $ Data.Version.makeVersion $ C.versionNumbers v

checkPackage :: Package -> WriterT [String] IO ()
checkPackage pkg = do
    v <- liftIO $ getPackageVersion pkg
    available <- liftIO $ getVersions (pkgName pkg)

    case M.lookup v available of
        Nothing         -> tell ["Version not on Hackage"]
        Just Deprecated -> tell ["Version has been deprecated"]
        Just Normal     -> return ()

    let compatible = [ v'
                     | v' <- M.keys available  -- versions available via Hackage...
                     , v' > v                  -- that are newer than the submodule...
                     , v' `isPvpCompatible` v  -- and are compatible with the submodule
                     ]
    case compatible of
        [] -> return ()
        vs -> tell ["At " <> showVersion v <> " but version " <> showVersion (maximum vs) <> " is available"]

    return ()

formatError :: Package -> String -> String
formatError pkg err = do
    pkgPath pkg <> ": " <> err

summarizeSubmodules :: [Package] -> IO ()
summarizeSubmodules pkgs = flip mapM_ pkgs $ \pkg -> do
    v <- getPackageVersion pkg
    putStrLn $ "    " <> C.unPackageName (pkgName pkg) <> " " <> showVersion v <> " @ " <> pkgPath pkg

main :: IO ()
main = do
    summarizeSubmodules packages
    errs <- mapM (\pkg -> map (pkg, ) <$> execWriterT (checkPackage pkg)) packages
    mapM_ (putStrLn . uncurry formatError) (concat errs)
    exitWith $ if null errs then ExitSuccess else ExitFailure 1


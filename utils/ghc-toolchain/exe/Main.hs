{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
import Data.Char (toUpper)
import Data.Maybe (isNothing,fromMaybe)
import System.Exit
import System.Console.GetOpt
import System.Environment
import System.FilePath ((</>))

import GHC.Platform.ArchOS

import GHC.Toolchain.Prelude
import GHC.Toolchain.Program
import GHC.Toolchain.Target
import GHC.Toolchain.PlatformDetails
import GHC.Toolchain.ParseTriple
import GHC.Toolchain.Utils

import GHC.Toolchain.Tools.Cc
import GHC.Toolchain.Tools.Cxx
import GHC.Toolchain.Tools.Cpp
import GHC.Toolchain.Tools.Link
import GHC.Toolchain.Tools.Ar
import GHC.Toolchain.Tools.Ranlib
import GHC.Toolchain.Tools.Nm
import GHC.Toolchain.Tools.MergeObjs
import GHC.Toolchain.Tools.Readelf

data Opts = Opts
    { optTriple    :: String
    , optTargetPrefix :: Maybe String
    , optLocallyExecutable :: Maybe Bool
    , optLlvmTriple :: Maybe String
    , optOutput    :: String
    , optCc        :: ProgOpt
    , optCxx       :: ProgOpt
    , optCpp       :: ProgOpt
    , optHsCpp     :: ProgOpt
    , optCcLink    :: ProgOpt
    , optAr        :: ProgOpt
    , optRanlib    :: ProgOpt
    , optNm        :: ProgOpt
    , optReadelf   :: ProgOpt
    , optMergeObjs :: ProgOpt
    , optWindres   :: ProgOpt
    , optUnregisterised :: Maybe Bool
    , optTablesNextToCode :: Maybe Bool
    , optUseLibFFIForAdjustors :: Maybe Bool
    , optLdOverride :: Maybe Bool
    , optVerbosity :: Int
    , optKeepTemp  :: Bool
    }

emptyOpts :: Opts
emptyOpts = Opts
    { optTriple    = ""
    , optTargetPrefix = Nothing
    , optLocallyExecutable = Nothing
    , optLlvmTriple = Nothing
    , optOutput    = ""
    , optCc        = po0
    , optCxx       = po0
    , optCpp       = po0
    , optHsCpp     = po0
    , optCcLink    = po0
    , optAr        = po0
    , optRanlib    = po0
    , optNm        = po0
    , optReadelf   = po0
    , optMergeObjs = po0
    , optWindres   = po0
    , optUnregisterised = Nothing
    , optTablesNextToCode = Nothing
    , optUseLibFFIForAdjustors = Nothing
    , optLdOverride = Nothing
    , optVerbosity = 1
    , optKeepTemp  = False
    }
  where
    po0 = emptyProgOpt

_optCc, _optCxx, _optCpp, _optHsCpp, _optCcLink, _optAr, _optRanlib, _optNm,
    _optReadelf, _optMergeObjs, _optWindres
    :: Lens Opts ProgOpt
_optCc      = Lens optCc      (\x o -> o {optCc=x})
_optCxx     = Lens optCxx     (\x o -> o {optCxx=x})
_optCpp     = Lens optCpp     (\x o -> o {optCpp=x})
_optHsCpp   = Lens optHsCpp   (\x o -> o {optHsCpp=x})
_optCcLink  = Lens optCcLink  (\x o -> o {optCcLink=x})
_optAr      = Lens optAr      (\x o -> o {optAr=x})
_optRanlib  = Lens optRanlib  (\x o -> o {optRanlib=x})
_optNm      = Lens optNm      (\x o -> o {optNm=x})
_optReadelf = Lens optReadelf (\x o -> o {optReadelf=x})
_optMergeObjs = Lens optMergeObjs (\x o -> o {optMergeObjs=x})
_optWindres = Lens optWindres (\x o -> o {optWindres=x})

_optTriple :: Lens Opts String
_optTriple = Lens optTriple (\x o -> o {optTriple=x})

_optLlvmTriple :: Lens Opts (Maybe String)
_optLlvmTriple = Lens optLlvmTriple (\x o -> o {optLlvmTriple=x})

_optOutput :: Lens Opts String
_optOutput = Lens optOutput (\x o -> o {optOutput=x})

_optTargetPrefix :: Lens Opts (Maybe String)
_optTargetPrefix = Lens optTargetPrefix (\x o -> o {optTargetPrefix=x})

_optLocallyExecutable, _optUnregisterised, _optTablesNextToCode, _optUseLibFFIForAdjustors, _optLdOvveride :: Lens Opts (Maybe Bool)
_optLocallyExecutable = Lens optLocallyExecutable (\x o -> o {optLocallyExecutable=x})
_optUnregisterised = Lens optUnregisterised (\x o -> o {optUnregisterised=x})
_optTablesNextToCode = Lens optTablesNextToCode (\x o -> o {optTablesNextToCode=x})
_optUseLibFFIForAdjustors = Lens optUseLibFFIForAdjustors (\x o -> o {optUseLibFFIForAdjustors=x})
_optLdOvveride = Lens optLdOverride (\x o -> o {optLdOverride=x})

_optVerbosity :: Lens Opts Int
_optVerbosity = Lens optVerbosity (\x o -> o {optVerbosity=x})

_optKeepTemp :: Lens Opts Bool
_optKeepTemp = Lens optKeepTemp (\x o -> o {optKeepTemp=x})

options :: [OptDescr (Opts -> Opts)]
options =
    [ tripleOpt
    , targetPrefixOpt
    , llvmTripleOpt
    , verbosityOpt
    , keepTempOpt
    , outputOpt
    ] ++
    concat
    [ enableDisable "unregisterised" "unregisterised backend" _optUnregisterised
    , enableDisable "tables-next-to-code" "info-tables-next-to-code optimisation" _optTablesNextToCode
    , enableDisable "libffi-adjustors" "the use of libffi for adjustors, even on platforms which have support for more efficient, native adjustor implementations." _optUseLibFFIForAdjustors
    , enableDisable "ld-override" "override gcc's default linker" _optLdOvveride
    , enableDisable "locally-executable" "the use of a target prefix which will be added to all tool names when searching for toolchain components" _optLocallyExecutable
    ] ++
    concat
    [ progOpts "cc" "C compiler" _optCc
    , progOpts "cpp" "C preprocessor" _optCpp
    , progOpts "hs-cpp" "Haskell C preprocessor" _optHsCpp
    , progOpts "cxx" "C++ compiler" _optCxx
    , progOpts "cc-link" "C compiler for linking" _optCcLink
    , progOpts "ar" "ar archiver" _optAr
    , progOpts "ranlib" "ranlib utility" _optRanlib
    , progOpts "nm" "nm archiver" _optNm
    , progOpts "readelf" "readelf utility" _optReadelf
    , progOpts "merge-objs" "linker for merging objects" _optMergeObjs
    , progOpts "windres" "windres utility" _optWindres
    ]
  where
    progOpts :: String -> String -> Lens Opts ProgOpt -> [OptDescr (Opts -> Opts)]
    progOpts progName description lens =
        [ Option [] [progName] (ReqArg (set (lens % _poPath) . progPath) metavar) ("Path of " ++ description)
        , Option [] [progName++"-opt"] (ReqArg (over (lens % _poFlags) . updatePoFlags) "OPTS") ("Flags to pass to " ++ progName)
        ]
      where
        metavar = map toUpper progName

        progPath "" = Nothing
        progPath p  = Just p

        -- Empty list of flags is as if it was unspecified
        updatePoFlags "" existingOpts      = existingOpts
        -- Otherwise prepend specified flags to existing flags or make new
        updatePoFlags newOpts Nothing      = Just [newOpts]
        updatePoFlags newOpts (Just eopts) = Just (newOpts:eopts)
        -- NB: By prepending, the resulting flags will match the left-to-right
        -- order they were passed in


    enableDisable :: String -> String -> Lens Opts (Maybe Bool) -> [OptDescr (Opts -> Opts)]
    enableDisable optName description lens =
        [ Option [] ["enable-" ++ optName] (NoArg (set lens (Just True))) ("Enable " ++ description)
        , Option [] ["disable-" ++ optName] (NoArg (set lens (Just False))) ("Disable " ++ description)
        ]

    tripleOpt = Option ['t'] ["triple"] (ReqArg (set _optTriple) "TRIPLE") "Target triple"
    llvmTripleOpt = Option [] ["llvm-triple"] (ReqArg (set _optLlvmTriple . Just) "LLVM-TRIPLE") "LLVM Target triple"

    targetPrefixOpt = Option ['T'] ["target-prefix"] (ReqArg (set _optTargetPrefix . Just) "PREFIX")
        "A target prefix which will be added to all tool names when searching for toolchain components"


    verbosityOpt = Option ['v'] ["verbose"] (OptArg f "N") "set output verbosity"
      where
        f mb = set _optVerbosity (parseVerbosity mb)
        parseVerbosity :: Maybe String -> Int
        parseVerbosity mb
          | Nothing <- mb        = 1
          | Just s <- mb
          , (n, ""):_ <- reads s = n
          | otherwise            = error "unparseable verbosity level"

    keepTempOpt = Option [] ["keep-temp"] (NoArg (set _optKeepTemp True))
        "do not remove temporary files"

    outputOpt = Option ['o'] ["output"] (ReqArg (set _optOutput) "OUTPUT")
        "The output path for the generated target toolchain configuration"

main :: IO ()
main = do
    argv <- getArgs
    let (opts0, _nonopts, errs) = getOpt RequireOrder options argv
    let opts = foldr (.) id opts0 emptyOpts
    case errs of
      [] -> do
          let env = Env { verbosity = optVerbosity opts
                        , targetPrefix = case optTargetPrefix opts of
                                           Just prefix -> Just prefix
                                           Nothing -> Just $ optTriple opts ++ "-"
                        , keepTemp = optKeepTemp opts
                        , canLocallyExecute = fromMaybe True (optLocallyExecutable opts)
                        , logContexts = []
                        }
          r <- runM env (run opts)
          case r of
            Left err -> print err >> exitWith (ExitFailure 2)
            Right () -> return ()
      _  -> do
        mapM_ putStrLn errs
        putStrLn $ usageInfo "ghc-toolchain" options
        exitWith (ExitFailure 1)

run :: Opts -> M ()
run opts = do
    tgt <- mkTarget opts
    logDebug $ "Final Target: " ++ show tgt
    let file = optOutput opts
    writeFile file (show tgt)

optional :: M a -> M (Maybe a)
optional k = fmap Just k <|> pure Nothing

registerisedSupported :: ArchOS -> Bool
registerisedSupported archOs =
    case archOS_arch archOs of
      ArchX86       -> True
      ArchX86_64    -> True
      ArchPPC       -> True
      ArchPPC_64 _  -> True
      ArchS390X     -> True
      ArchARM _ _ _ -> True
      ArchAArch64   -> True
      ArchRISCV64   -> True
      ArchWasm32    -> True
      ArchJavaScript -> True
      _             -> False

determineUnregisterised :: ArchOS -> Maybe Bool -> M Bool
determineUnregisterised archOs userReq =
    case userReq of
      Just False -- user requested registerised build
        | not regSupported -> throwE "GHC doesn't support registerised compilation on this architecture"
        | otherwise        -> return False
      Just True            -> return True
      Nothing -- user wasn't explicit, do registerised if we support it
        | regSupported     -> return False
        | otherwise        -> return True
  where
    regSupported = registerisedSupported archOs

tablesNextToCodeSupported :: ArchOS -> Bool
tablesNextToCodeSupported archOs =
    case archOS_arch archOs of
      ArchPPC      -> False
      ArchPPC_64 _ -> False
      ArchS390X    -> False
      _            -> True

determineTablesNextToCode
    :: ArchOS
    -> Bool       -- ^ unregisterised
    -> Maybe Bool -- ^ user flag
    -> M Bool
determineTablesNextToCode archOs unreg userReq =
    case userReq of
      Just True
        | not tntcSupported
                        -> throwE "Tables-next-to-code not supported by this platform"
        | unreg         -> throwE "Tables-next-to-code cannot be used with unregisterised code generator"
        | otherwise     -> return True
      Just False        -> return False
      Nothing           -> pure tntcSupported
  where
    tntcSupported = tablesNextToCodeSupported archOs

determineUseLibFFIForAdjustors :: ArchOS
                               -> Maybe Bool -- ^ Enable/disable option --libffi-adjustors
                               -> M Bool
determineUseLibFFIForAdjustors archOs mb = checking "whether to use libffi for adjustors" $
  case mb of
    Just True ->
      -- The user explicitly requested it
      pure True
    _    ->
      -- If don't have a native adjustor implementation we use libffi
      pure (not . archHasNativeAdjustors $ archOS_arch archOs)

-- | Do we implement a native adjustor implementation (i.e. found in @rts/adjustors@) for this 'Arch'?
archHasNativeAdjustors :: Arch -> Bool
archHasNativeAdjustors = \case
  ArchX86    -> True
  ArchX86_64 -> True
  _          -> False


-- | The platforms which we attempt to override ld
ldOverrideWhitelist :: ArchOS -> Bool
ldOverrideWhitelist a =
  case archOS_OS a of
    OSLinux   -> True
    OSMinGW32 -> True
    _ -> False


mkTarget :: Opts -> M Target
mkTarget opts = do
    -- Use Llvm target if specified, otherwise use triple as llvm target
    let tgtLlvmTarget = fromMaybe (optTriple opts) (optLlvmTriple opts)
    cc0 <- findCc tgtLlvmTarget (optCc opts)
    cxx <- findCxx tgtLlvmTarget (optCxx opts)
    cpp <- findCpp (optCpp opts) cc0
    hsCpp <- findHsCpp (optHsCpp opts) cc0
    (archOs, tgtVendor) <- parseTriple cc0 (optTriple opts)
    cc <- addPlatformDepCcFlags archOs cc0
    readelf <- optional $ findReadelf (optReadelf opts)
    ccLink <- findCcLink tgtLlvmTarget (optCcLink opts) (ldOverrideWhitelist archOs && fromMaybe True (optLdOverride opts)) archOs cc readelf

    ar <- findAr tgtVendor (optAr opts)
    -- TODO: We could have
    -- ranlib <- if arNeedsRanlib ar
    --              then Just <$> findRanlib (optRanlib opts)
    --              else return Nothing
    -- but in order to match the configure output, for now we do
    ranlib <- Just <$> findRanlib (optRanlib opts)

    nm <- findNm (optNm opts)
    mergeObjs <- optional $ findMergeObjs (optMergeObjs opts) cc ccLink nm

    when (isNothing mergeObjs && not (arSupportsDashL ar)) $
      throwE "Neither a object-merging tool (e.g. ld -r) nor an ar that supports -L is available"

    -- Windows-specific utilities
    windres <-
        case archOS_OS archOs of
          OSMinGW32 -> do
            windres <- findProgram "windres" (optWindres opts) ["windres"]
            return (Just windres)
          _ -> return Nothing

    -- various other properties of the platform
    tgtWordSize <- checkWordSize cc
    tgtEndianness <- checkEndianness cc
    tgtSymbolsHaveLeadingUnderscore <- checkLeadingUnderscore cc nm
    tgtSupportsSubsectionsViaSymbols <- checkSubsectionsViaSymbols archOs cc
    tgtSupportsIdentDirective <- checkIdentDirective cc
    tgtSupportsGnuNonexecStack <- checkGnuNonexecStack archOs cc

    -- code generator configuration
    tgtUnregisterised <- determineUnregisterised archOs (optUnregisterised opts)
    tgtTablesNextToCode <-
        determineTablesNextToCode archOs tgtUnregisterised (optTablesNextToCode opts)
    tgtUseLibffi <- determineUseLibFFIForAdjustors archOs (optUseLibFFIForAdjustors opts)
    when tgtUnregisterised $ do
        -- The via-C code generator requires these
        let prog = "int main(int argc, char** argv) { return 0; }"
            via_c_args = ["-fwrapv", "-fno-builtin"]
        forM_ via_c_args $ \arg -> checking ("support of "++arg) $ withTempDir $ \dir -> do
            let cc' = over (_ccProgram % _prgFlags) (++ [arg]) cc
            compileC cc' (dir </> "test.o") prog
            return ()

    let t = Target { tgtArchOs = archOs
                   , tgtVendor
                   , tgtLocallyExecutable = fromMaybe True (optLocallyExecutable opts)
                   , tgtCCompiler = cc
                   , tgtCxxCompiler = cxx
                   , tgtCPreprocessor = cpp
                   , tgtHsCPreprocessor = hsCpp
                   , tgtAr = ar
                   , tgtCCompilerLink = ccLink
                   , tgtRanlib = ranlib
                   , tgtNm = nm
                   , tgtMergeObjs = mergeObjs
                   , tgtWindres = windres
                   , tgtWordSize
                   , tgtEndianness
                   , tgtUnregisterised
                   , tgtTablesNextToCode
                   , tgtUseLibffiForAdjustors = tgtUseLibffi
                   , tgtSymbolsHaveLeadingUnderscore
                   , tgtSupportsSubsectionsViaSymbols
                   , tgtSupportsIdentDirective
                   , tgtSupportsGnuNonexecStack
                   , tgtLlvmTarget
                   }
    return t

--- ROMES:TODO: fp_settings.m4 in general which I don't think was ported completely (e.g. the basenames and windows llvm-XX and such)

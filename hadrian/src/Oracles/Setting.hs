module Oracles.Setting (
    configFile,
    -- * Settings
    Setting (..), setting, getSetting,
    ToolchainSetting (..), settingsFileSetting,

    -- * Helpers
    ghcCanonVersion, cmdLineLengthLimit, hostSupportsRPaths, topDirectory,
    libsuf, ghcVersionStage, bashPath, targetStage, crossStage, queryTarget, queryTargetTarget,

    -- ** Target platform things
    anyTargetOs, anyTargetArch, anyHostOs,
    isElfTarget, isOsxTarget, isWinTarget, isJsTarget, isArmTarget,
    targetArmVersion,
    ghcWithInterpreter
    ) where

import System.Directory
import System.Info.Extra
import Hadrian.Expression
import Hadrian.Oracles.TextFile
import Hadrian.Oracles.Path
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (runMaybeT)

import Base

import GHC.Toolchain
import GHC.Platform.ArchOS

-- | Each 'Setting' comes from the file @hadrian/cfg/system.config@, generated
-- by the @configure@ script from the input file @hadrian/cfg/system.config.in@.
-- For example, the line
--
-- > target-os = mingw32
--
-- sets the value of the setting 'TargetOs'. The action 'setting' 'TargetOs'
-- looks up the value of the setting and returns the string @"mingw32"@,
-- tracking the result in the Shake database.
--
-- * ROMES:TODO: How to handle target-platform-full?
data Setting = CursesIncludeDir
             | CursesLibDir
             | DynamicExtension
             | FfiIncludeDir
             | FfiLibDir
             | GhcMajorVersion
             | GhcMinorVersion
             | GhcPatchLevel
             | GhcVersion
             | GhcSourcePath
             | LlvmMinVersion
             | LlvmMaxVersion
             | GmpIncludeDir
             | GmpLibDir
             | IconvIncludeDir
             | IconvLibDir
             | LibdwIncludeDir
             | LibdwLibDir
             | LibnumaIncludeDir
             | LibnumaLibDir
             | LibZstdIncludeDir
             | LibZstdLibDir
             | ProjectGitCommitId
             | ProjectName
             | ProjectVersion
             | ProjectVersionInt
             | ProjectVersionMunged
             | ProjectVersionForLib
             | ProjectPatchLevel
             | ProjectPatchLevel1
             | ProjectPatchLevel2
             | SystemGhc
             | TargetPlatformFull
             | BourneShell
             | EmsdkVersion

-- TODO compute solely in Hadrian, removing these variables' definitions
-- from aclocal.m4 whenever they can be calculated from other variables
-- already fed into Hadrian.

-- | All 'ToolchainSetting's are computed by the ghc-toolchain utility for configuring toolchains.
-- This used to be defined by 'FP_SETTINGS' in aclocal.m4.
--
-- TODO: We should be able to drop this completely, after moving all the toolchain settings to ghc-toolchain
-- Move to ghc-toolchain and to the Target files generated by configure and ghc-toolchain
-- * First we will get rid of DistroMinGW when we fix the windows build
data ToolchainSetting
    = ToolchainSetting_OtoolCommand
    | ToolchainSetting_InstallNameToolCommand
    | ToolchainSetting_LlcCommand
    | ToolchainSetting_OptCommand
    | ToolchainSetting_LlvmAsCommand
    | ToolchainSetting_DistroMinGW

-- | Look up the value of a 'Setting' in @cfg/system.config@, tracking the
-- result.
setting :: Setting -> Action String
setting key = lookupSystemConfig $ case key of
    CursesIncludeDir   -> "curses-include-dir"
    CursesLibDir       -> "curses-lib-dir"
    DynamicExtension   -> "dynamic-extension"
    FfiIncludeDir      -> "ffi-include-dir"
    FfiLibDir          -> "ffi-lib-dir"
    GhcMajorVersion    -> "ghc-major-version"
    GhcMinorVersion    -> "ghc-minor-version"
    GhcPatchLevel      -> "ghc-patch-level"
    GhcVersion         -> "ghc-version"
    GhcSourcePath      -> "ghc-source-path"
    LlvmMinVersion     -> "llvm-min-version"
    LlvmMaxVersion     -> "llvm-max-version"
    GmpIncludeDir      -> "gmp-include-dir"
    GmpLibDir          -> "gmp-lib-dir"
    IconvIncludeDir    -> "iconv-include-dir"
    IconvLibDir        -> "iconv-lib-dir"
    LibdwIncludeDir    -> "libdw-include-dir"
    LibdwLibDir        -> "libdw-lib-dir"
    LibnumaIncludeDir  -> "libnuma-include-dir"
    LibnumaLibDir      -> "libnuma-lib-dir"
    LibZstdIncludeDir  -> "libzstd-include-dir"
    LibZstdLibDir      -> "libzstd-lib-dir"
    ProjectGitCommitId -> "project-git-commit-id"
    ProjectName        -> "project-name"
    ProjectVersion     -> "project-version"
    ProjectVersionMunged -> "project-version-munged"
    ProjectVersionForLib -> "project-version-for-lib"
    ProjectVersionInt  -> "project-version-int"
    ProjectPatchLevel  -> "project-patch-level"
    ProjectPatchLevel1 -> "project-patch-level1"
    ProjectPatchLevel2 -> "project-patch-level2"
    SystemGhc          -> "system-ghc"
    TargetPlatformFull -> "target-platform-full"
    BourneShell        -> "bourne-shell"
    EmsdkVersion       -> "emsdk-version"

-- | Look up the value of a 'SettingList' in @cfg/system.config@, tracking the
-- result.
-- See Note [tooldir: How GHC finds mingw on Windows]
-- ROMES:TODO: This should be queryTargetTargetConfig
settingsFileSetting :: ToolchainSetting -> Action String
settingsFileSetting key = lookupSystemConfig $ case key of
    ToolchainSetting_OtoolCommand           -> "settings-otool-command"
    ToolchainSetting_InstallNameToolCommand -> "settings-install_name_tool-command"
    ToolchainSetting_LlcCommand             -> "settings-llc-command"
    ToolchainSetting_OptCommand             -> "settings-opt-command"
    ToolchainSetting_LlvmAsCommand          -> "settings-llvm-as-command"
    ToolchainSetting_DistroMinGW            -> "settings-use-distro-mingw" -- ROMES:TODO: This option doesn't seem to be in ghc-toolchain yet. It corresponds to EnableDistroToolchain

-- | An expression that looks up the value of a 'Setting' in @cfg/system.config@,
-- tracking the result.
getSetting :: Setting -> Expr c b String
getSetting = expr . setting

-- | The path to a Bourne shell interpreter.
bashPath :: Action FilePath
bashPath = setting BourneShell

isWinTarget :: Stage -> Action Bool
isWinTarget stage = anyTargetOs stage [OSMinGW32]

isJsTarget :: Stage -> Action Bool
isJsTarget stage = anyTargetArch stage [ArchJavaScript]

isOsxTarget :: Stage -> Action Bool
isOsxTarget stage = anyTargetOs stage [OSDarwin]

isArmTarget :: Stage -> Action Bool
isArmTarget stage = queryTargetTarget stage (isARM . archOS_arch . tgtArchOs)

-- | Check whether the host OS setting matches one of the given strings.
anyHostOs :: [OS] -> Action Bool
anyHostOs oss = (`elem` oss) <$> queryHostTarget (archOS_OS . tgtArchOs)

-- | Check whether the target architecture setting matches one of the given
-- strings.
anyTargetArch :: Stage -> [Arch] -> Action Bool
anyTargetArch stage archs = (`elem` archs) <$> queryTargetTarget stage (archOS_arch . tgtArchOs)

-- | Check whether the target OS setting matches one of the given strings.
anyTargetOs :: Stage -> [OS] -> Action Bool
anyTargetOs stage oss = (`elem` oss) <$> queryTargetTarget stage (archOS_OS . tgtArchOs)

-- | Check whether the target OS uses the ELF object format.
isElfTarget :: Stage -> Action Bool
isElfTarget stage = queryTargetTarget stage (osElfTarget . archOS_OS . tgtArchOs)

-- | Check whether the host OS supports the @-rpath@ linker option when
-- using dynamic linking.
--
-- ROMES:TODO: Whether supports -rpath should be determined by ghc-toolchain
--
-- TODO: Windows supports lazy binding (but GHC doesn't currently support
--       dynamic way on Windows anyways).
hostSupportsRPaths :: Action Bool
hostSupportsRPaths = queryHostTarget (\t -> let os = archOS_OS (tgtArchOs t)
                                             in osElfTarget os || osMachOTarget os)

-- | Check whether the target supports GHCi.
ghcWithInterpreter :: Stage -> Action Bool
ghcWithInterpreter stage = do
    goodOs <- anyTargetOs stage [ OSMinGW32, OSLinux, OSSolaris2 -- TODO "cygwin32"?,
                          , OSFreeBSD, OSDragonFly, OSNetBSD, OSOpenBSD
                          , OSDarwin, OSKFreeBSD ]
    goodArch <- (||) <$>
                anyTargetArch stage [ ArchX86, ArchX86_64, ArchPPC
                              , ArchAArch64, ArchS390X
                              , ArchPPC_64 ELF_V1, ArchPPC_64 ELF_V2
                              , ArchRISCV64 ]
                              <*> isArmTarget stage
    return $ goodOs && goodArch

-- | Which variant of the ARM architecture is the target (or 'Nothing' if not
-- ARM)?
targetArmVersion :: Stage -> Action (Maybe ArmISA)
targetArmVersion stage = runMaybeT $ do
    ArchARM isa _ _ <- lift $ queryTargetTarget stage (archOS_arch . tgtArchOs)
    return isa

-- | Canonicalised GHC version number, used for integer version comparisons. We
-- expand 'GhcMinorVersion' to two digits by adding a leading zero if necessary.
ghcCanonVersion :: Action String
ghcCanonVersion = do
    ghcMajorVersion <- setting GhcMajorVersion
    ghcMinorVersion <- setting GhcMinorVersion
    let leadingZero = [ '0' | length ghcMinorVersion == 1 ]
    return $ ghcMajorVersion ++ leadingZero ++ ghcMinorVersion

-- | Absolute path to the GHC source tree.
topDirectory :: Action FilePath
topDirectory = do
    x <- fixAbsolutePathOnWindows =<< setting GhcSourcePath
    canonicalize x
  where
    -- We must canonicalize as the source directory may be accessed via a symlink. See #22451.
    canonicalize = if isWindows then return else liftIO . canonicalizePath

ghcVersionStage :: Stage -> Action String
ghcVersionStage (Stage0 {}) = setting GhcVersion
ghcVersionStage _      = setting ProjectVersion

-- | The file suffix used for libraries of a given build 'Way'. For example,
-- @_p.a@ corresponds to a static profiled library, and @-ghc7.11.20141222.so@
-- is a dynamic vanilla library. Why do we need GHC version number in the
-- dynamic suffix? Here is a possible reason: dynamic libraries are placed in a
-- single giant directory in the load path of the dynamic linker, and hence we
-- must distinguish different versions of GHC. In contrast, static libraries
-- live in their own per-package directory and hence do not need a unique
-- filename. We also need to respect the system's dynamic extension, e.g. @.dll@
-- or @.so@.
libsuf :: Stage -> Way -> Action String
libsuf st way
    | not (wayUnit Dynamic way) = return (waySuffix way ++ ".a") -- e.g., _p.a
    | otherwise = do
        extension <- setting DynamicExtension -- e.g., .dll or .so
        version   <- ghcVersionStage st -- e.g. 8.4.4 or 8.9.xxxx
        let suffix = waySuffix (removeWayUnit Dynamic way)
        return (suffix ++ "-ghc" ++ version ++ extension)

-- Build libraries for this stage targetting this Target
-- For example, we want to build RTS with stage1 for the host target as we produce a host executable with stage1  (which cross-compiles to stage2)
targetStage :: Stage -> Action Target
targetStage (Stage0 {}) = getHostTarget
targetStage (Stage1 {}) = getHostTarget
targetStage (Stage2 {}) = getTargetTarget
targetStage (Stage3 {}) = getTargetTarget

queryTarget :: Stage -> (Target -> a) -> (Expr c b a)
queryTarget s f = expr (f <$> targetStage s)

queryTargetTarget :: Stage -> (Target -> a) -> Action a
queryTargetTarget s f = f <$> targetStage s

-- | A 'Stage' is a cross-stage if the produced compiler is a cross-compiler.
crossStage :: Stage -> Action Bool
crossStage st = do
  st_target <- targetStage (succStage st)
  st_host   <- targetStage st
  return (targetPlatformTriple st_target /= targetPlatformTriple st_host)


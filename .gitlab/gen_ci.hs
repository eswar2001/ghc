#!/usr/bin/env cabal
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- cabal:
build-depends: base, aeson >= 1.8.1, containers, bytestring
-}

import Data.Coerce
import Data.String (String)
import Data.Aeson as A
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.ByteString.Lazy as B hiding (putStrLn)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as S
import System.Environment

{-
Note [Generating the CI pipeline]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This script is responsible for generating the majority of jobs in the CI pipeline.
In particular, it generates all the standard build configurations which perform a
full build and test of the compiler.

There are broadly three categories of job:

* validate - jobs run on every MR, these are typically validate builds.
* nightly - jobs run once per day on the master branch
* release - jobs for producing release artifacts, these are perf builds.

Basically, for each suitable combination of architecture and operating system these three
jobs are generated which run either.

In reality things are a bit more complicated because

* validate - we run some additional validation jobs which have no corresponding release artifacts
* nightly  - Some builds are only run on nightly, rather than also validate, to
             relieve pressure on CI
* release  - Not all jobs are run in release pipelines, only those which we
             produce release artifacts for.

The job specification can be seen at the bottom of this file in the 'jobs' variable.

The generated jobs assume certain things about the configuration file they are included
into. For example

* The DOCKER_REV variable must be set (which specifies the versions of the docker images)

Things will go very quickly wrong if you don't have the right variables set, the
testing logic in `ci.sh` contains more dependencies on these global variables.

Generating the CI configuration
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In order to regenerate the CI configuration you need to run the ./generate_jobs
script which adds a module header and also formats the output JSON with jq.

Other CI jobs
~~~~~~~~~~~~~

Not all the jobs in the CI pipeline are generated by this script. There are quite a
few ad-hoc jobs (which don't fit into the build/test with hadrian) model. For example

* linters
* hadrian/ghci
* One test which builds with the make build system (until we remove it #17527)

All these definitions are found in the .gitlab-ci.yaml file directly.


Note [Consumers of artifacts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The generated names for the jobs is important as there are a few downstream consumers
of the jobs artifacts. Therefore some care should be taken if changing the generated
names of jobs to update these other places.

1. Fedora33 jobs are required by head.hackage
2. The fetch-gitlab release utility pulls release artifacts from the
3. The ghc-head-from script downloads release artifacts based on a pipeline change.
4. Some subsequent CI jobs have explicit dependencies (for example docs-tarball, perf, perf-nofib)

-}

-----------------------------------------------------------------------------
-- Definition of a BuildConfig (options which affect the binaries which are in the bindist)
-----------------------------------------------------------------------------

-- | Operating system
data Opsys
  = Linux LinuxDistro
  | Darwin
  | FreeBSD13
  | Windows deriving (Eq)

data LinuxDistro
  = Debian11 | Debian10 | Debian9 | Fedora33 | Ubuntu2004 | Centos7 | Alpine deriving (Eq)

data Arch = Amd64 | AArch64 | ARMv7 | I386

data BignumBackend = Native | Gmp deriving Eq

bignumString :: BignumBackend -> String
bignumString Gmp = "gmp"
bignumString Native = "native"

-- | A BuildConfig records all the options which can be modified to affect the
-- bindists produced by the compiler.
data BuildConfig
  = BuildConfig { withDwarf      :: Bool
                , unregisterised :: Bool
                , buildFlavour   :: BaseFlavour
                , bignumBackend  :: BignumBackend
                , llvmBootstrap  :: Bool
                , withAssertions :: Bool
                , withNuma       :: Bool
                , crossTarget    :: Maybe String
                , crossEmulator  :: Maybe String
                , fullyStatic    :: Bool
                , tablesNextToCode :: Bool
                , threadSanitiser :: Bool
                }

-- Extra arguments to pass to ./configure due to the BuildConfig
configureArgsStr :: BuildConfig -> String
configureArgsStr bc = intercalate " " $
  ["--enable-unregisterised"| unregisterised bc ]
  ++ ["--disable-tables-next-to-code" | not (tablesNextToCode bc) ]
  ++ ["--with-intree-gmp" | Just _ <- pure (crossTarget bc) ]

-- Compute the hadrian flavour from the BuildConfig
mkJobFlavour :: BuildConfig -> Flavour
mkJobFlavour BuildConfig{..} = Flavour buildFlavour opts
  where
    opts = [Llvm | llvmBootstrap] ++
           [Dwarf | withDwarf] ++
           [FullyStatic | fullyStatic] ++
           [ThreadSanitiser | threadSanitiser]

data Flavour = Flavour BaseFlavour [FlavourTrans]

data FlavourTrans = Llvm | Dwarf | FullyStatic | ThreadSanitiser

data BaseFlavour = Release | Validate | SlowValidate

-----------------------------------------------------------------------------
-- Build Configs
-----------------------------------------------------------------------------

-- | A standard build config
vanilla :: BuildConfig
vanilla = BuildConfig
  { withDwarf      = False
  , unregisterised = False
  , buildFlavour   = Validate
  , bignumBackend  = Gmp
  , llvmBootstrap  = False
  , withAssertions = False
  , withNuma = False
  , crossTarget = Nothing
  , crossEmulator = Nothing
  , fullyStatic = False
  , tablesNextToCode = True
  , threadSanitiser = False
  }

nativeInt :: BuildConfig
nativeInt = vanilla { bignumBackend = Native }

dwarf :: BuildConfig
dwarf = vanilla { withDwarf = True }

unreg :: BuildConfig
unreg = vanilla { unregisterised = True }

releaseConfig :: BuildConfig
releaseConfig = vanilla { buildFlavour = Release }

debug :: BuildConfig
debug = vanilla { buildFlavour = SlowValidate
                , withAssertions = True
                -- WithNuma so at least one job tests Numa
                , withNuma = True
                }

static :: BuildConfig
static = vanilla { fullyStatic = True }

staticNativeInt :: BuildConfig
staticNativeInt = static { bignumBackend = Native }

crossConfig :: String       -- ^ target triple
            -> Maybe String -- ^ emulator for testing
            -> BuildConfig
crossConfig triple emulator =
    vanilla { crossTarget = Just triple
            , crossEmulator = emulator
            }

llvm :: BuildConfig
llvm = vanilla { llvmBootstrap = True }

tsan :: BuildConfig
tsan = vanilla { threadSanitiser = True }

noTntc :: BuildConfig
noTntc = vanilla { tablesNextToCode = False }

-----------------------------------------------------------------------------
-- Platform specific variables
-----------------------------------------------------------------------------

-- | These tags have to match what we call the runners on gitlab
runnerTag :: Arch -> Opsys -> String
runnerTag arch (Linux distro) =
  case arch of
    Amd64   -> "x86_64-linux"
    AArch64 -> "aarch64-linux"
    ARMv7   -> "armv7-linux"
    I386    -> "x86_64-linux"
runnerTag AArch64 Darwin = "aarch64-darwin"
runnerTag Amd64 Darwin = "x86_64-darwin-m1"
runnerTag Amd64 Windows = "new-x86_64-windows"
runnerTag Amd64 FreeBSD13 = "x86_64-freebsd13"

tags :: Arch -> Opsys -> BuildConfig -> [String]
tags arch opsys _bc = [runnerTag arch opsys] -- Tag for which runners we can use

-- These names are used to find the docker image so they have to match what is
-- in the docker registry.
distroName :: LinuxDistro -> String
distroName Debian11  = "deb11"
distroName Debian10   = "deb10"
distroName Debian9   = "deb9"
distroName Fedora33  = "fedora33"
distroName Ubuntu2004 = "ubuntu20_04"
distroName Centos7    = "centos7"
distroName Alpine     = "alpine3_12"

opsysName :: Opsys -> String
opsysName (Linux distro) = "linux-" ++ distroName distro
opsysName Darwin = "darwin"
opsysName FreeBSD13 = "freebsd13"
opsysName Windows = "windows"

archName :: Arch -> String
archName Amd64 = "x86_64"
archName AArch64 = "aarch64"
archName ARMv7 = "armv7"
archName I386  = "i386"

binDistName :: Arch -> Opsys -> BuildConfig -> String
binDistName arch opsys bc = "ghc-" ++ testEnv arch opsys bc

-- | Test env should create a string which changes whenever the 'BuildConfig' changes.
-- Either the change is reflected by modifying the flavourString or directly (as is
-- the case for settings which affect environment variables)
testEnv :: Arch -> Opsys -> BuildConfig -> String
testEnv arch opsys bc = intercalate "-" $
                        [ archName arch
                        , opsysName opsys ]
                        ++ ["int_" ++ bignumString (bignumBackend bc) | bignumBackend bc /= Gmp]
                        ++ ["unreg" | unregisterised bc ]
                        ++ ["numa"  | withNuma bc ]
                        ++ ["no_tntc"  | not (tablesNextToCode bc) ]
                        ++ ["cross_"++triple  | Just triple <- pure $ crossTarget bc ]
                        ++ [flavourString (mkJobFlavour bc)]

-- | The hadrian flavour string we are going to use for this build
flavourString :: Flavour -> String
flavourString (Flavour base trans) = baseString base ++ concatMap (("+" ++) . flavourString) trans
  where
    baseString Release = "release"
    baseString Validate = "validate"
    baseString SlowValidate = "slow-validate"

    flavourString Llvm = "llvm"
    flavourString Dwarf = "debug_info"
    flavourString FullyStatic = "fully_static"
    flavourString ThreadSanitiser = "thread_sanitizer"

-- The path to the docker image (just for linux builders)
dockerImage :: Arch -> Opsys -> Maybe String
dockerImage arch (Linux distro) =
    Just image
  where
    image = mconcat
      [ "registry.gitlab.haskell.org/ghc/ci-images/"
      , archName arch
      , "-linux-"
      , distroName distro
      , ":$DOCKER_REV"
      ]
dockerImage _ _ = Nothing

-----------------------------------------------------------------------------
-- Platform specific variables
-----------------------------------------------------------------------------

-- The variables map is a monoidal map so that we don't ever accidentally lose
-- variables settings by silently overwriting when merging. At the end these variables
-- are combinated together with spaces if they are set multiple times. This may
-- produce nonsense but it's easier to debug that silently overwriting.
--
-- The "proper" solution would be to use a dependent monoidal map where each key specifies
-- the combination behaviour of it's values. Ie, whether setting it multiple times is an error
-- or they should be combined.
newtype MonoidalMap k v = MonoidalMap (Map k v)
    deriving (Eq, Show, Functor, ToJSON)

instance (Ord k, Semigroup v) => Semigroup (MonoidalMap k v) where
    (MonoidalMap a) <> (MonoidalMap b) = MonoidalMap (Map.unionWith (<>) a b)

instance (Ord k, Semigroup v) => Monoid (MonoidalMap k v) where
    mempty = MonoidalMap (Map.empty)

mminsertWith :: Ord k => (a -> a -> a) -> k -> a -> MonoidalMap k a -> MonoidalMap k a
mminsertWith f k v (MonoidalMap m) = MonoidalMap (Map.insertWith f k v m)

type Variables = MonoidalMap String [String]

(=:) :: String -> String -> Variables
a =: b = MonoidalMap (Map.singleton a [b])

opsysVariables :: Arch -> Opsys -> Variables
opsysVariables _ FreeBSD13 = mconcat
  [ -- N.B. we use iconv from ports as I see linker errors when we attempt
    -- to use the "native" iconv embedded in libc as suggested by the
    -- porting guide [1].
    -- [1] https://www.freebsd.org/doc/en/books/porters-handbook/using-iconv.html)
    "CONFIGURE_ARGS" =:  "--with-gmp-includes=/usr/local/include --with-gmp-libraries=/usr/local/lib --with-iconv-includes=/usr/local/include --with-iconv-libraries=/usr/local/lib"
  , "HADRIAN_ARGS" =: "--docs=no-sphinx"
  , "GHC_VERSION" =: "9.2.2"
  , "CABAL_INSTALL_VERSION" =: "3.6.2.0"
    -- We workaround an upstream clang/FreeBSD bug (#21986) by using clang++14 rather than
    -- the default clang-13 compiler.
  , "CC" =: "clang14"
  , "CXX" =: "clang++14"
  ]
opsysVariables ARMv7 (Linux distro) =
  distroVariables distro <>
  mconcat [ "CONFIGURE_ARGS" =: "--host=armv7-linux-gnueabihf --build=armv7-linux-gnueabihf --target=armv7-linux-gnueabihf"
            -- N.B. We disable ld.lld explicitly here because it appears to fail
            -- non-deterministically on ARMv7. See #18280.
          , "LD" =: "ld.gold"
          , "GccUseLdOpt" =: "-fuse-ld=gold"
            -- Awkwardly, this appears to be necessary to work around a
            -- live-lock exhibited by the CPython (at least in 3.9 and 3.8)
            -- interpreter on ARMv7
          , "HADRIAN_ARGS" =: "--test-verbose=3"
          ]
opsysVariables _ (Linux distro) = distroVariables distro
opsysVariables AArch64 (Darwin {}) =
  mconcat [ "NIX_SYSTEM" =: "aarch64-darwin"
          , "MACOSX_DEPLOYMENT_TARGET" =: "11.0"
          , "LANG" =: "en_US.UTF-8"
          , "CONFIGURE_ARGS" =: "--with-intree-gmp --with-system-libffi"
          -- Fonts can't be installed on darwin
          , "HADRIAN_ARGS" =: "--docs=no-sphinx"
          ]
opsysVariables Amd64 (Darwin {}) =
  mconcat [ "NIX_SYSTEM" =: "x86_64-darwin"
          , "MACOSX_DEPLOYMENT_TARGET" =: "10.10"
          -- "# Only Sierra and onwards supports clock_gettime. See #12858"
          , "ac_cv_func_clock_gettime" =: "no"
          -- # Only newer OS Xs support utimensat. See #17895
          , "ac_cv_func_utimensat" =: "no"
          , "LANG" =: "en_US.UTF-8"
          , "CONFIGURE_ARGS" =: "--with-intree-gmp --with-system-libffi"
          -- Fonts can't be installed on darwin
          , "HADRIAN_ARGS" =: "--docs=no-sphinx"

          ]
opsysVariables _ (Windows {}) =
  mconcat [ "MSYSTEM" =: "MINGW64"
          , "HADRIAN_ARGS" =: "--docs=no-sphinx"
          , "LANG" =: "en_US.UTF-8"
          , "CABAL_INSTALL_VERSION" =: "3.2.0.0"
          , "GHC_VERSION" =: "9.2.2" ]
opsysVariables _ _ = mempty


distroVariables :: LinuxDistro -> Variables
distroVariables Alpine = mconcat
  [ -- Due to #20266
    "CONFIGURE_ARGS" =: "--disable-ld-override"
  , "INSTALL_CONFIGURE_ARGS" =: "--disable-ld-override"
  , "HADRIAN_ARGS" =: "--docs=no-sphinx"
    -- encoding004: due to lack of locale support
    -- T10458, ghcilink002: due to #17869
    -- linker_unload_native: due to musl not supporting any means of probing dynlib dependencies
    -- (see Note [Object unloading]).
  , "BROKEN_TESTS" =: "encoding004 T10458 ghcilink002 linker_unload_native"
  ]
distroVariables Centos7 = mconcat [
  "HADRIAN_ARGS" =: "--docs=no-sphinx"
  ]
distroVariables Fedora33 = mconcat
  -- LLC/OPT do not work for some reason in our fedora images
  -- These tests fail with this error: T11649 T5681 T7571 T8131b
  -- +/opt/llvm/bin/opt: /lib64/libtinfo.so.5: no version information available (required by /opt/llvm/bin/opt)
-- +/opt/llvm/bin/llc: /lib64/libtinfo.so.5: no version information available (required by /opt/llvm/bin/llc)
  [ "LLC" =: "/bin/false"
  , "OPT" =: "/bin/false"
  ]
distroVariables _ = mempty

-----------------------------------------------------------------------------
-- Cache settings, what to cache and when can we share the cache
-----------------------------------------------------------------------------

data Cache
  = Cache { cacheKey :: String
          , cachePaths :: [String]
          }

-- The cache doesn't depend on the BuildConfig because we only cache the cabal store.
mkCacheKey :: Arch -> Opsys -> String
mkCacheKey arch opsys = archName arch <> "-" <> opsysName opsys <> "-$CACHE_REV"

instance ToJSON Cache where
  toJSON Cache {..} = object
    [ "key" A..= cacheKey
    , "paths" A..= cachePaths
    ]

-----------------------------------------------------------------------------
-- Artifacts, what to store and how long for
-----------------------------------------------------------------------------

data Artifacts
  = Artifacts { artifactPaths :: [String]
              , junitReport   :: String
              , expireIn      :: String
              , artifactsWhen :: ArtifactsWhen
              }

instance ToJSON Artifacts where
  toJSON Artifacts{..} = object
    [ "reports" A..= object
      [ "junit" A..= junitReport
      ]
    , "expire_in" A..= expireIn
    , "paths" A..= artifactPaths
    , "when" A..= artifactsWhen
    ]

data ArtifactsWhen = ArtifactsOnSuccess | ArtifactsOnFailure | ArtifactsAlways

instance ToJSON ArtifactsWhen where
  toJSON ArtifactsOnSuccess = "on_success"
  toJSON ArtifactsOnFailure = "on_failure"
  toJSON ArtifactsAlways = "always"

-----------------------------------------------------------------------------
-- Rules, when do we run a job
-----------------------------------------------------------------------------

-- Data structure which records the condition when a job is run.
data OnOffRules = OnOffRules { rule_set :: Set Rule -- ^ The set of enabled rules
                             , when :: ManualFlag   -- ^ The additional condition about when to run this job.
                             }

-- The initial set of rules where all rules are disabled and the job is always run.
emptyRules :: OnOffRules
emptyRules = OnOffRules S.empty OnSuccess

-- When to run the job
data ManualFlag = Manual -- ^ Only run the job when explicitly triggered by a user
                | OnSuccess -- ^ Always run it, if the rules pass (the default)
                deriving Eq

enableRule :: Rule -> OnOffRules -> OnOffRules
enableRule r (OnOffRules o m) = OnOffRules (S.insert r o) m

manualRule :: OnOffRules -> OnOffRules
manualRule rules = rules { when = Manual }

-- Given 'OnOffRules', returns a list of ALL rules with their toggled status.
-- For example, even if you don't explicitly disable a rule it will end up in the
-- rule list with the OFF state.
enumRules :: OnOffRules -> [OnOffRule]
enumRules o = map lkup rules
  where
    enabled_rules = rule_set o
    lkup r = OnOffRule (if S.member r enabled_rules then On else Off) r


data OnOffRule = OnOffRule OnOff Rule

data OnOff = On | Off

instance ToJSON ManualFlag where
  toJSON Manual = "manual"
  toJSON OnSuccess = "on_success"

instance ToJSON OnOffRules where
  toJSON rules = toJSON [(object ([
    "if" A..= and_all (map one_rule (enumRules rules))
    , "when" A..= toJSON (when rules)]
    -- Necessary to stop manual jobs stopping pipeline progress
    -- https://docs.gitlab.com/ee/ci/yaml/#rulesallow_failure
    ++
    ["allow_failure" A..= True | when rules == Manual ]))]

    where
      one_rule (OnOffRule onoff r) = ruleString onoff r
      parens s = "(" ++ s ++ ")"
      and_all rs = intercalate " && " (map parens rs)

-- | A Rule corresponds to some condition which must be satisifed in order to
-- run the job.
data Rule = FastCI       -- ^ Run this job when the fast-ci label is set
          | ReleaseOnly  -- ^ Only run this job in a release pipeline
          | Nightly      -- ^ Only run this job in the nightly pipeline
          | LLVMBackend  -- ^ Only run this job when the "LLVM backend" label is present
          | FreeBSDLabel -- ^ Only run this job when the "FreeBSD" label is set.
          | ARMLabel    -- ^ Only run this job when the "ARM" label is set.
          | Disable      -- ^ Don't run this job.
          deriving (Bounded, Enum, Ord, Eq)

-- A constant evaluating to True because gitlab doesn't support "true" in the
-- expression language.
true :: String
true =  "\"true\" == \"true\""
-- A constant evaluating to False because gitlab doesn't support "true" in the
-- expression language.
false :: String
false = "\"disabled\" != \"disabled\""

-- Convert the state of the rule into a string that gitlab understand.
ruleString :: OnOff -> Rule -> String
ruleString On FastCI = true
ruleString Off FastCI = "$CI_MERGE_REQUEST_LABELS !~ /.*fast-ci.*/"
ruleString On LLVMBackend = "$CI_MERGE_REQUEST_LABELS =~ /.*LLVM backend.*/"
ruleString Off LLVMBackend = true
ruleString On FreeBSDLabel = "$CI_MERGE_REQUEST_LABELS =~ /.*FreeBSD.*/"
ruleString Off FreeBSDLabel = true
ruleString On ARMLabel = "$CI_MERGE_REQUEST_LABELS =~ /.*ARM.*/"
ruleString Off ARMLabel = true
ruleString On ReleaseOnly = "$RELEASE_JOB == \"yes\""
ruleString Off ReleaseOnly = "$RELEASE_JOB != \"yes\""
ruleString On Nightly = "$NIGHTLY"
ruleString Off Nightly = "$NIGHTLY == null"
ruleString On Disable = false
ruleString Off Disable = true

-- Enumeration of all the rules
rules :: [Rule]
rules = [minBound .. maxBound]

-- | A 'Job' is the description of a single job in a gitlab pipeline. The
-- job contains all the information about how to do the build but can be further
-- modified with information about when to run jobs, which variables to set for
-- certain platforms and so on.
data Job
  = Job { jobStage :: String
        , jobNeeds :: [String]
        , jobTags :: [String]
        , jobAllowFailure :: Bool
        , jobScript :: [String]
        , jobAfterScript :: [String]
        , jobDockerImage :: Maybe String
        , jobVariables :: Variables
        , jobDependencies :: [String]
        , jobArtifacts :: Artifacts
        , jobCache :: Cache
        , jobRules :: OnOffRules
        }

instance ToJSON Job where
  toJSON Job{..} = object
    [ "stage" A..= jobStage
    -- Convoluted to avoid download artifacts from ghci job
    -- https://docs.gitlab.com/ee/ci/yaml/#needsartifacts
    , "needs" A..= map (\j -> object [ "job" A..= j, "artifacts" A..= False ]) jobNeeds
    , "dependencies" A..= jobDependencies
    , "image" A..= jobDockerImage
    , "tags" A..= jobTags
    , "allow_failure" A..= jobAllowFailure
    -- Joining up variables like this may well be the wrong thing to do but
    -- at least it doesn't lose information silently by overriding.
    , "variables" A..= fmap (intercalate " ") jobVariables
    , "artifacts" A..= jobArtifacts
    , "cache" A..= jobCache
    , "after_script" A..= jobAfterScript
    , "script" A..= jobScript
    , "rules" A..= jobRules
    ]

-- | Build a job description from the system description and 'BuildConfig'
job :: Arch -> Opsys -> BuildConfig -> (String, Job)
job arch opsys buildConfig = (jobName, Job {..})
  where
    jobRules = emptyRules

    jobName = testEnv arch opsys buildConfig

    jobTags = tags arch opsys buildConfig

    jobDockerImage = dockerImage arch opsys

    jobScript
      | Windows <- opsys
      = [ "bash .gitlab/ci.sh setup"
        , "bash .gitlab/ci.sh configure"
        , "bash .gitlab/ci.sh build_hadrian"
        , "bash .gitlab/ci.sh test_hadrian" ]
      | otherwise
      = [ "find libraries -name config.sub -exec cp config.sub {} \\;" | Darwin == opsys ] ++
        [ "sudo chown ghc:ghc -R ." | Linux {} <- [opsys]] ++
        [ ".gitlab/ci.sh setup"
        , ".gitlab/ci.sh configure"
        , ".gitlab/ci.sh build_hadrian"
        , ".gitlab/ci.sh test_hadrian"
        ]

    jobAfterScript
      | Windows <- opsys =
      [ "bash .gitlab/ci.sh save_cache"
      , "bash .gitlab/ci.sh clean"
      ]
      | otherwise =
      [ ".gitlab/ci.sh save_cache"
      , ".gitlab/ci.sh clean"
      , "cat ci_timings" ]

    jobFlavour = mkJobFlavour buildConfig

    jobDependencies = []
    jobVariables = mconcat
      [ opsysVariables arch opsys
      ,"TEST_ENV" =: testEnv arch opsys buildConfig
      , "BIN_DIST_NAME" =: binDistName arch opsys buildConfig
      , "BUILD_FLAVOUR" =: flavourString jobFlavour
      , "BIGNUM_BACKEND" =: bignumString (bignumBackend buildConfig)
      , "CONFIGURE_ARGS" =: configureArgsStr buildConfig
      , maybe mempty ("CROSS_TARGET" =:) (crossTarget buildConfig)
      , maybe mempty ("CROSS_EMULATOR" =:) (crossEmulator buildConfig)
      , if withNuma buildConfig then "ENABLE_NUMA" =: "1" else mempty
      ]

    jobArtifacts = Artifacts
      { junitReport = "junit.xml"
      , expireIn = "2 weeks"
      , artifactPaths = [binDistName arch opsys buildConfig ++ ".tar.xz"
                        ,"junit.xml"]
      , artifactsWhen = ArtifactsAlways
      }

    jobCache
        -- N.B. We have temporarily disabled cabal-install store caching on
        -- Windows due to #21347.
      | Windows <- opsys =
          Cache { cachePaths = [], cacheKey = "no-caching" }
      | otherwise = Cache
          { cachePaths = [ "cabal-cache", "toolchain" ]
          , cacheKey = mkCacheKey arch opsys

          }

    jobAllowFailure = False
    jobStage = "full-build"
    jobNeeds = ["hadrian-ghc-in-ghci"]

---------------------------------------------------------------------------
-- Job Modifiers
---------------------------------------------------------------------------

-- Generic modification functions

-- | Modify all jobs in a 'JobGroup'
modifyJobs :: (a -> a) -> JobGroup a -> JobGroup a
modifyJobs f = fmap f

-- | Modify just the validate jobs in a 'JobGroup'
modifyValidateJobs :: (a -> a) -> JobGroup a -> JobGroup a
modifyValidateJobs f jg = jg { v = f <$> (v jg) }

-- Generic helpers

addJobRule :: Rule -> Job -> Job
addJobRule r j = j { jobRules = enableRule r (jobRules j) }

addVariable :: String -> String -> Job -> Job
addVariable k v j = j { jobVariables = mminsertWith (++) k [v] (jobVariables j) }

-- Building the standard jobs
--
-- | Make a normal validate CI job
validate :: Arch -> Opsys -> BuildConfig -> (String, Job)
validate arch opsys bc =
  job arch opsys bc

-- | Make a normal nightly CI job
nightly arch opsys bc =
  let (n, j) = job arch opsys bc
  in ("nightly-" ++ n, addJobRule Nightly . keepArtifacts "8 weeks" . highCompression $ j)

-- | Make a normal release CI job
release arch opsys bc =
  let (n, j) = job arch opsys (bc { buildFlavour = Release })
  in ("release-" ++ n, addJobRule ReleaseOnly . keepArtifacts "1 year" . ignorePerfFailures . highCompression $ j)

-- Specific job modification functions

-- | Mark a job as requiring a manual trigger.
manual :: Job -> Job
manual j = j { jobRules = manualRule (jobRules j) }

-- | Mark a job as allowed to fail
allowFailure :: Job -> Job
allowFailure j = j { jobAllowFailure = True }

-- | Modify the time the job keeps its artifacts for
keepArtifacts :: String -> Job -> Job
keepArtifacts l j = j { jobArtifacts = (jobArtifacts j) { expireIn = l } }

-- | Ignore performance test failures for this job
ignorePerfFailures :: Job -> Job
ignorePerfFailures = addVariable "IGNORE_PERF_FAILURES" "all"

-- | Use a higher compression level to produce the job bindists (slower but produces
-- smaller results)
highCompression :: Job -> Job
highCompression = addVariable "XZ_OPT" "-9"

-- | Mark the validate job to run in fast-ci mode
fastCI :: JobGroup Job -> JobGroup Job
fastCI = modifyValidateJobs (addJobRule FastCI)

-- | Mark a group of jobs as allowed to fail.
allowFailureGroup :: JobGroup Job -> JobGroup Job
allowFailureGroup = modifyJobs allowFailure

-- | Add a 'Rule' to just the validate job, for example, only run a job if a certain
-- label is set.
addValidateRule :: Rule -> JobGroup Job -> JobGroup Job
addValidateRule t = modifyValidateJobs (addJobRule t)

-- | Don't run the validate job, normally used to alleviate CI load by marking
-- jobs which are unlikely to fail (ie different linux distros)
disableValidate :: JobGroup Job -> JobGroup Job
disableValidate = addValidateRule Disable

-- Jobs are grouped into either triples or pairs depending on whether the
-- job is just validate and nightly, or also release.
data JobGroup a = StandardTriple { v :: (String, a)
                                 , n :: (String, a)
                                 , r :: (String, a) }
                | ValidateOnly   { v :: (String, a)
                                 , n :: (String, a) } deriving Functor

rename :: (String -> String) -> JobGroup a -> JobGroup a
rename f (StandardTriple (nv, v) (nn, n) (nr, r)) = StandardTriple (f nv, v) (f nn, n) (f nr, r)
rename f (ValidateOnly (nv, v) (nn, n)) = ValidateOnly (f nv, v) (f nn, n)

-- | Construct a 'JobGroup' which consists of a validate, nightly and release build with
-- a specific config.
standardBuildsWithConfig :: Arch -> Opsys -> BuildConfig -> JobGroup Job
standardBuildsWithConfig a op bc =
  StandardTriple (validate a op bc)
                 (nightly a op bc)
                 (release a op bc)

-- | Construct a 'JobGroup' which consists of a validate, nightly and release builds with
-- the 'vanilla' config.
standardBuilds :: Arch -> Opsys -> JobGroup Job
standardBuilds a op = standardBuildsWithConfig a op vanilla

-- | Construct a 'JobGroup' which just consists of a validate and nightly build. We don't
-- produce releases for these jobs.
validateBuilds :: Arch -> Opsys -> BuildConfig -> JobGroup Job
validateBuilds a op bc = ValidateOnly (validate a op bc) (nightly a op bc)

flattenJobGroup :: JobGroup a -> [(String, a)]
flattenJobGroup (StandardTriple a b c) = [a,b,c]
flattenJobGroup (ValidateOnly a b) = [a, b]


-- | Specification for all the jobs we want to build.
jobs :: Map String Job
jobs = Map.fromList $ concatMap flattenJobGroup $
     [ disableValidate (standardBuilds Amd64 (Linux Debian10))
     , (standardBuildsWithConfig Amd64 (Linux Debian10) dwarf)
     , (validateBuilds Amd64 (Linux Debian10) nativeInt)
     , fastCI (validateBuilds Amd64 (Linux Debian10) unreg)
     , fastCI (validateBuilds Amd64 (Linux Debian10) debug)
     , modifyValidateJobs manual tsan_jobs
     , modifyValidateJobs manual (validateBuilds Amd64 (Linux Debian10) noTntc)
     , addValidateRule LLVMBackend (validateBuilds Amd64 (Linux Debian10) llvm)

     , disableValidate (standardBuilds Amd64 (Linux Debian11))
     -- We still build Deb9 bindists for now due to Ubuntu 18 and Linux Mint 19
     -- not being at EOL until April 2023 and they still need tinfo5.
     , disableValidate (standardBuilds Amd64 (Linux Debian9))
     , disableValidate (standardBuilds Amd64 (Linux Ubuntu2004))
     , disableValidate (standardBuilds Amd64 (Linux Centos7))
     -- Fedora33 job is always built with perf so there's one job in the normal
     -- validate pipeline which is built with perf.
     , (standardBuildsWithConfig Amd64 (Linux Fedora33) releaseConfig)
     -- This job is only for generating head.hackage docs
     , hackage_doc_job (disableValidate (standardBuildsWithConfig Amd64 (Linux Fedora33) releaseConfig))
     , disableValidate (standardBuildsWithConfig Amd64 (Linux Fedora33) dwarf)
     , fastCI (standardBuilds Amd64 Windows)
     , disableValidate (standardBuildsWithConfig Amd64 Windows nativeInt)
     , standardBuilds Amd64 Darwin
     , addValidateRule FreeBSDLabel (standardBuilds Amd64 FreeBSD13)
     , standardBuilds AArch64 Darwin
     , standardBuilds AArch64 (Linux Debian10)
     , allowFailureGroup (addValidateRule ARMLabel (standardBuilds ARMv7 (Linux Debian10)))
     , standardBuilds I386 (Linux Debian9)
     , allowFailureGroup (standardBuildsWithConfig Amd64 (Linux Alpine) static)
     , disableValidate (allowFailureGroup (standardBuildsWithConfig Amd64 (Linux Alpine) staticNativeInt))
     , validateBuilds Amd64 (Linux Debian11) (crossConfig "aarch64-linux-gnu" (Just "qemu-aarch64 -L /usr/aarch64-linux-gnu"))
     ]

  where
    hackage_doc_job = rename (<> "-hackage") . modifyJobs (addVariable "HADRIAN_ARGS" "--haddock-base-url")

    tsan_jobs =
      modifyJobs
        ( addVariable "TSAN_OPTIONS" "suppressions=$CI_PROJECT_DIR/rts/.tsan-suppressions"
         -- Haddock is large enough to make TSAN choke without massive quantities of
         -- memory.
        . addVariable "HADRIAN_ARGS" "--docs=none") $
      validateBuilds Amd64 (Linux Debian10) tsan

main = do
  as <- getArgs
  (case as of
    [] -> B.putStrLn
    (fp:_) -> B.writeFile fp)
    (A.encode jobs)


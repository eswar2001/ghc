module GHC.Tc.Types.TH where
import GHCi.RemoteTypes
import qualified Language.Haskell.TH as TH
import GHC.Hs.Expr
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Evidence
import GHC.Utils.Outputable
import GHC.Prelude
import GHC.Utils.Panic
import GHC.Tc.Types.TcRef
---------------------------
-- Template Haskell stages and levels
---------------------------


data SpliceType = Typed | Untyped

data ThStage    -- See Note [Template Haskell state diagram]
                -- and Note [Template Haskell levels] in GHC.Tc.Gen.Splice
    -- Start at:   Comp
    -- At bracket: wrap current stage in Brack
    -- At splice:  currently Brack: return to previous stage
    --             currently Comp/Splice: compile and run
  = Splice SpliceType -- Inside a top-level splice
                      -- This code will be run *at compile time*;
                      --   the result replaces the splice
                      -- Binding level = 0

  | RunSplice (TcRef [ForeignRef (TH.Q ())])
      -- Set when running a splice, i.e. NOT when renaming or typechecking the
      -- Haskell code for the splice. See Note [RunSplice ThLevel].
      --
      -- Contains a list of mod finalizers collected while executing the splice.
      --
      -- 'addModFinalizer' inserts finalizers here, and from here they are taken
      -- to construct an @HsSpliced@ annotation for untyped splices. See Note
      -- [Delaying modFinalizers in untyped splices] in GHC.Rename.Splice.
      --
      -- For typed splices, the typechecker takes finalizers from here and
      -- inserts them in the list of finalizers in the global environment.
      --
      -- See Note [Collecting modFinalizers in typed splices] in "GHC.Tc.Gen.Splice".

  | Comp        -- Ordinary Haskell code
                -- Binding level = 1

  | Brack                       -- Inside brackets
      ThStage                   --   Enclosing stage
      PendingStuff

data PendingStuff
  = RnPendingUntyped              -- Renaming the inside of an *untyped* bracket
      (TcRef [PendingRnSplice])   -- Pending splices in here

  | RnPendingTyped                -- Renaming the inside of a *typed* bracket

  | TcPending                     -- Typechecking the inside of a typed bracket
      (TcRef [PendingTcSplice])   --   Accumulate pending splices here
      (TcRef WantedConstraints)   --     and type constraints here
      QuoteWrapper                -- A type variable and evidence variable
                                  -- for the overall monad of
                                  -- the bracket. Splices are checked
                                  -- against this monad. The evidence
                                  -- variable is used for desugaring
                                  -- `lift`.


topStage, topAnnStage, topSpliceStage :: ThStage
topStage       = Comp
topAnnStage    = Splice Untyped
topSpliceStage = Splice Untyped

instance Outputable ThStage where
   ppr (Splice _)    = text "Splice"
   ppr (RunSplice _) = text "RunSplice"
   ppr Comp          = text "Comp"
   ppr (Brack s _)   = text "Brack" <> parens (ppr s)

type ThLevel = Int
    -- NB: see Note [Template Haskell levels] in GHC.Tc.Gen.Splice
    -- Incremented when going inside a bracket,
    -- decremented when going inside a splice
    -- NB: ThLevel is one greater than the 'n' in Fig 2 of the
    --     original "Template meta-programming for Haskell" paper

impLevel, outerLevel :: ThLevel
impLevel = 0    -- Imported things; they can be used inside a top level splice
outerLevel = 1  -- Things defined outside brackets

thLevel :: ThStage -> ThLevel
thLevel (Splice _)    = 0
thLevel Comp          = 1
thLevel (Brack s _)   = thLevel s + 1
thLevel (RunSplice _) = panic "thLevel: called when running a splice"
                        -- See Note [RunSplice ThLevel].

{- Note [RunSplice ThLevel]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The 'RunSplice' stage is set when executing a splice, and only when running a
splice. In particular it is not set when the splice is renamed or typechecked.

'RunSplice' is needed to provide a reference where 'addModFinalizer' can insert
the finalizer (see Note [Delaying modFinalizers in untyped splices]), and
'addModFinalizer' runs when doing Q things. Therefore, It doesn't make sense to
set 'RunSplice' when renaming or typechecking the splice, where 'Splice',
'Brack' or 'Comp' are used instead.

-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

module GHC.StgToJS.Types where

import GHC.Prelude

import GHC.JS.Syntax
import GHC.JS.Make

import GHC.Stg.Syntax
import GHC.Core.TyCon

import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Types.Var
import GHC.Types.ForeignCall

import Control.Monad.Trans.State.Strict
import GHC.Utils.Outputable (Outputable (..), text, SDocContext, (<+>), ($$))

import GHC.Data.FastString

import GHC.Unit.Module

import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.ByteString as BS
import           Data.Monoid
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           Control.DeepSeq

type G = StateT GenState IO

data GenState = GenState
  { gsSettings  :: StgToJSConfig          -- ^ codegen settings, read-only
  , gsModule    :: !Module                -- ^ current module
  , gsId        :: !Int                   -- ^ unique number for the id generator
  , gsIdents    :: !IdCache               -- ^ hash consing for identifiers from a Unique
  , gsUnfloated :: !(UniqFM Id CgStgExpr) -- ^ unfloated arguments
  , gsGroup     :: GenGroupState          -- ^ state for the current binding group
  , gsGlobal    :: [JStat]                -- ^ global (per module) statements (gets included when anything else from the module is used)
  }

-- | the state relevant for the current binding group
data GenGroupState = GenGroupState
  { ggsToplevelStats :: [JStat]        -- ^ extra toplevel statements for the binding group
  , ggsClosureInfo   :: [ClosureInfo]  -- ^ closure metadata (info tables) for the binding group
  , ggsStatic        :: [StaticInfo]   -- ^ static (CAF) data in our binding group
  , ggsStack         :: [StackSlot]    -- ^ stack info for the current expression
  , ggsStackDepth    :: Int            -- ^ current stack depth
  , ggsExtraDeps     :: Set OtherSymb  -- ^ extra dependencies for the linkable unit that contains this group
  , ggsGlobalIdCache :: GlobalIdCache
  , ggsForeignRefs   :: [ForeignJSRef]
  }

data StgToJSConfig = StgToJSConfig
  -- flags
  { csInlinePush      :: !Bool
  , csInlineBlackhole :: !Bool
  , csInlineLoadRegs  :: !Bool
  , csInlineEnter     :: !Bool
  , csInlineAlloc     :: !Bool
  , csTraceRts        :: !Bool
  , csAssertRts       :: !Bool
  , csDebugAlloc      :: !Bool
  , csTraceForeign    :: !Bool
  , csProf            :: !Bool -- ^ Profiling enabled
  , csRuntimeAssert   :: !Bool -- ^ Enable runtime assertions
  -- settings
  , csContext         :: !SDocContext
  }

data ClosureInfo = ClosureInfo
  { ciVar     :: FastString -- ^ object being infod
  , ciRegs    :: CIRegs     -- ^ things in registers when this is the next closure to enter
  , ciName    :: FastString -- ^ friendly name for printing
  , ciLayout  :: CILayout   -- ^ heap/stack layout of the object
  , ciType    :: CIType     -- ^ type of the object, with extra info where required
  , ciStatic  :: CIStatic   -- ^ static references of this object
  }
  deriving stock (Eq, Show, Generic)

data CIRegs
  = CIRegsUnknown
  | CIRegs { ciRegsSkip  :: Int       -- ^ unused registers before actual args start
           , ciRegsTypes :: [VarType] -- ^ args
           }
  deriving stock (Eq, Ord, Show, Generic)

instance NFData CIRegs

data CILayout
  = CILayoutVariable            -- layout stored in object itself, first position from the start
  | CILayoutUnknown             -- fixed size, but content unknown (for example stack apply frame)
      { layoutSize :: !Int
      }
  | CILayoutFixed               -- whole layout known
      { layoutSize :: !Int      -- closure size in array positions, including entry
      , layout     :: [VarType]
      }
  deriving stock (Eq, Ord, Show, Generic)

instance NFData CILayout

data CIType
  = CIFun { citArity :: !Int  -- ^ function arity
          , citRegs  :: !Int  -- ^ number of registers for the args
          }
  | CIThunk
  | CICon { citConstructor :: !Int }
  | CIPap
  | CIBlackhole
  | CIStackFrame
  deriving stock (Eq, Ord, Show, Generic)

instance NFData CIType

-- | Static references that must be kept alive
newtype CIStatic = CIStaticRefs { staticRefs :: [FastString] }
  deriving stock   (Eq, Generic)
  deriving newtype (Semigroup, Monoid, Show)

-- | static refs: array = references, null = nothing to report
--   note: only works after all top-level objects have been created
instance ToJExpr CIStatic where
  toJExpr (CIStaticRefs [])  = null_ -- [je| null |]
  toJExpr (CIStaticRefs rs)  = toJExpr (map TxtI rs)

-- function argument and free variable types
data VarType
  = PtrV     -- pointer = reference to heap object (closure object)
  | VoidV    -- no fields
  -- | FloatV   -- one field -- no single precision supported
  | DoubleV  -- one field
  | IntV     -- one field
  | LongV    -- two fields
  | AddrV    -- a pointer not to the heap: two fields, array + index
  | RtsObjV  -- some RTS object from GHCJS (for example TVar#, MVar#, MutVar#, Weak#)
  | ObjV     -- some JS object, user supplied, be careful around these, can be anything
  | ArrV     -- boxed array
  deriving stock (Eq, Ord, Enum, Bounded, Show, Generic)

instance NFData VarType

instance ToJExpr VarType where
  toJExpr = toJExpr . fromEnum

data IdType
  = IdPlain
  | IdEntry
  | IdConEntry
  deriving (Enum, Eq, Ord)

data IdKey
  = IdKey !Int !Int !IdType
  deriving (Eq, Ord)

data OtherSymb
  = OtherSymb !Module !FastString
  deriving Eq

instance Ord OtherSymb where
  compare (OtherSymb m1 t1) (OtherSymb m2 t2)
    = stableModuleCmp m1 m2 <> lexicalCompareFS t1 t2

newtype IdCache = IdCache (M.Map IdKey Ident)
newtype GlobalIdCache = GlobalIdCache (M.Map Ident (IdKey, Id))

data StackSlot
  = SlotId !Id !Int
  | SlotUnknown
  deriving (Eq, Ord)


data StaticInfo = StaticInfo
  { siVar    :: !FastString -- ^ global object
  , siVal    :: !StaticVal     -- ^ static initialization
  , siCC     :: !(Maybe Ident) -- ^ optional CCS name
  } deriving stock (Eq, Show, Typeable, Generic)

data StaticVal
  = StaticFun     !FastString [StaticArg]
    -- ^ heap object for function
  | StaticThunk   !(Maybe (FastString,[StaticArg]))
    -- ^ heap object for CAF (field is Nothing when thunk is initialized in an
    -- alternative way, like string thunks through h$str)
  | StaticUnboxed !StaticUnboxed
    -- ^ unboxed constructor (Bool, Int, Double etc)
  | StaticData    !FastString [StaticArg]
    -- ^ regular datacon app
  | StaticList    [StaticArg] (Maybe FastString)
    -- ^ list initializer (with optional tail)
  deriving stock (Eq, Show, Generic)

data StaticUnboxed
  = StaticUnboxedBool         !Bool
  | StaticUnboxedInt          !Integer
  | StaticUnboxedDouble       !SaneDouble
  | StaticUnboxedString       !BS.ByteString
  | StaticUnboxedStringOffset !BS.ByteString
  deriving stock (Eq, Ord, Show, Generic)

instance NFData StaticUnboxed

data StaticArg
  = StaticObjArg !FastString     -- ^ reference to a heap object
  | StaticLitArg !StaticLit             -- ^ literal
  | StaticConArg !FastString [StaticArg] -- ^ unfloated constructor
  deriving stock (Eq, Show, Generic)

instance Outputable StaticArg where
  ppr x = text (show x)

data StaticLit
  = BoolLit   !Bool
  | IntLit    !Integer
  | NullLit
  | DoubleLit !SaneDouble -- should we actually use double here?
  | StringLit !FastString
  | BinLit    !BS.ByteString
  | LabelLit  !Bool !FastString -- ^ is function pointer, label (also used for string / binary init)
  deriving (Eq, Show, Generic)

instance Outputable StaticLit where
  ppr x = text (show x)


instance ToJExpr StaticLit where
  toJExpr (BoolLit b)           = toJExpr b
  toJExpr (IntLit i)            = toJExpr i
  toJExpr NullLit               = null_
  toJExpr (DoubleLit d)         = toJExpr (unSaneDouble d)
  toJExpr (StringLit t)         = app (mkFastString "h$str") [toJExpr t]
  toJExpr (BinLit b)            = app (mkFastString "h$rstr") [toJExpr (map toInteger (BS.unpack b))]
  toJExpr (LabelLit _isFun lbl) = var lbl

data ForeignJSRef = ForeignJSRef
  { foreignRefSrcSpan  :: !FastString
  , foreignRefPattern  :: !FastString
  , foreignRefSafety   :: !Safety
  , foreignRefCConv    :: !CCallConv
  , foreignRefArgs     :: ![FastString]
  , foreignRefResult   :: !FastString
  } deriving stock (Generic)

-- | data used to generate one ObjUnit in our object file
data LinkableUnit = LinkableUnit
  { luStat         :: BS.ByteString -- ^ serialized JS AST
  , luIdExports    :: [Id]          -- ^ exported names from haskell identifiers
  , luOtherExports :: [FastString]  -- ^ other exports
  , luIdDeps       :: [Id]          -- ^ identifiers this unit depends on
  , luPseudoIdDeps :: [Unique]      -- ^ pseudo-id identifiers this unit depends on (fixme)
  , luOtherDeps    :: [OtherSymb]   -- ^ symbols not from a haskell id that this unit depends on
  , luRequired     :: Bool          -- ^ always link this unit
  , luForeignRefs  :: [ForeignJSRef]
  }

-- | Typed expression
data TypedExpr = TypedExpr
  { typex_typ  :: !PrimRep
  , typex_expr :: [JExpr]
  }

instance Outputable TypedExpr where
  ppr x = text "TypedExpr: " <+> ppr (typex_expr x)
          $$  text "PrimReps: " <+> ppr (typex_typ x)

data PrimRes
  = PrimInline JStat  -- ^ primop is inline, result is assigned directly
  | PRPrimCall JStat  -- ^ primop is async call, primop returns the next
                      --     function to run. result returned to stack top in registers

data ExprResult
  = ExprCont
  | ExprInline (Maybe [JExpr])
  deriving (Eq)

newtype ExprValData = ExprValData [JExpr]
  deriving newtype (Eq)

-- closure types
data ClosureType
  = Thunk
  | Fun
  | Pap
  | Con
  | Blackhole
  | StackFrame
  deriving (Show, Eq, Ord, Enum, Bounded)

ctNum :: ClosureType -> Int
ctNum Fun        = 1
ctNum Con        = 2
ctNum Thunk      = 0
ctNum Pap        = 3
ctNum Blackhole  = 5
ctNum StackFrame = -1

ctJsName :: ClosureType -> String
ctJsName = \case
  Thunk      -> "CLOSURE_TYPE_THUNK"
  Fun        -> "CLOSURE_TYPE_FUN"
  Pap        -> "CLOSURE_TYPE_PAP"
  Con        -> "CLOSURE_TYPE_CON"
  Blackhole  -> "CLOSURE_TYPE_BLACKHOLE"
  StackFrame -> "CLOSURE_TYPE_STACKFRAME"

instance ToJExpr ClosureType where
  toJExpr e = toJExpr (ctNum e)


data ThreadStatus
  = Running
  | Blocked
  | Finished
  | Died
  deriving (Show, Eq, Ord, Enum, Bounded)

threadStatusNum :: ThreadStatus -> Int
threadStatusNum = \case
  Running  -> 0
  Blocked  -> 1
  Finished -> 16
  Died     -> 17

threadStatusJsName :: ThreadStatus -> String
threadStatusJsName = \case
  Running  -> "THREAD_RUNNING"
  Blocked  -> "THREAD_BLOCKED"
  Finished -> "THREAD_FINISHED"
  Died     -> "THREAD_DIED"

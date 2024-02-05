{-# LANGUAGE DerivingStrategies #-}

{-

Describes predicates as they are considered by the solver.

-}

module GHC.Core.Predicate (
  Pred(..), classifyPredType,
  isPredTy, isEvVarType,

  -- Equality predicates
  EqRel(..), eqRelRole,
  isEqPred, isReprEqPred, isEqClassPred, isCoVarType,
  getEqPredTys, getEqPredTys_maybe, getEqPredRole,
  predTypeEqRel,
  mkNomEqPred, mkReprEqPred, mkEqPred, mkEqPredRole,

  -- Class predicates
  mkClassPred, isDictTy, typeDeterminesValue,
  isClassPred, isEqualityClass, isCTupleClass,
  getClassPredTys, getClassPredTys_maybe,
  classMethodTy, classMethodInstTy,

  -- Implicit parameters
  isIPLikePred, mentionsIP, isIPTyCon, isIPClass,
  isCallStackTy, isCallStackPred, isCallStackPredTy,
  isExceptionContextPred,
  isIPPred_maybe,

  -- Evidence variables
  DictId, isEvVar, isDictId,

  -- * Well-scoped free variables
  scopedSort, tyCoVarsOfTypeWellScoped,
  tyCoVarsOfTypesWellScoped

  ) where

import GHC.Prelude

import GHC.Core.Type
import GHC.Core.Class
import GHC.Core.TyCo.Compare( eqType )
import GHC.Core.TyCo.FVs( tyCoVarsOfTypeList, tyCoVarsOfTypesList )
import GHC.Core.TyCon
import GHC.Core.TyCon.RecWalk
import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Core.Multiplicity ( scaledThing )

import GHC.Builtin.Names
import GHC.Builtin.Types.Prim( eqPrimTyCon, eqReprPrimTyCon )

import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Data.FastString

-- | A predicate in the solver. The solver tries to prove Wanted predicates
-- from Given ones.
data Pred

  -- | A typeclass predicate.
  = ClassPred Class [Type]

  -- | A type equality predicate, (t1 ~#N t2) or (t1 ~#R t2)
  | EqPred EqRel Type Type

  -- | An irreducible predicate.
  | IrredPred PredType

  -- | A quantified predicate.
  --
  -- See Note [Quantified constraints] in GHC.Tc.Solver.Solve
  | ForAllPred [TyVar] [PredType] PredType

  -- NB: There is no TuplePred case
  --     Tuple predicates like (Eq a, Ord b) are just treated
  --     as ClassPred, as if we had a tuple class with two superclasses
  --        class (c1, c2) => CTuple2 c1 c2

classifyPredType :: PredType -> Pred
classifyPredType ev_ty = case splitTyConApp_maybe ev_ty of
    Just (tc, [_, _, ty1, ty2])
      | tc `hasKey` eqReprPrimTyConKey -> EqPred ReprEq ty1 ty2
      | tc `hasKey` eqPrimTyConKey     -> EqPred NomEq  ty1 ty2

    Just (tc, tys)
      | Just clas <- tyConClass_maybe tc
      -> ClassPred clas tys

    _ | (tvs, rho) <- splitForAllTyCoVars ev_ty
      , (theta, pred) <- splitFunTys rho
      , not (null tvs && null theta)
      -> ForAllPred tvs (map scaledThing theta) pred

      | otherwise
      -> IrredPred ev_ty

-- --------------------- Dictionary types ---------------------------------

mkClassPred :: Class -> [Type] -> PredType
mkClassPred clas tys = mkTyConApp (classTyCon clas) tys

isDictTy :: Type -> Bool
-- True of dictionaries (Eq a) and
--         dictionary functions (forall a. Eq a => Eq [a])
-- See Note [Type determines value]
-- See #24370 (and the isDictId call in GHC.HsToCore.Binds.decomposeRuleLhs)
--     for why it's important to catch dictionary bindings
isDictTy ty = isClassPred pred
  where
    (_, pred) = splitInvisPiTys ty

typeDeterminesValue :: Type -> Bool
-- See Note [Type determines value]
typeDeterminesValue ty = isDictTy ty && not (isIPLikePred ty)

getClassPredTys :: HasDebugCallStack => PredType -> (Class, [Type])
getClassPredTys ty = case getClassPredTys_maybe ty of
        Just (clas, tys) -> (clas, tys)
        Nothing          -> pprPanic "getClassPredTys" (ppr ty)

getClassPredTys_maybe :: PredType -> Maybe (Class, [Type])
getClassPredTys_maybe ty = case splitTyConApp_maybe ty of
        Just (tc, tys) | Just clas <- tyConClass_maybe tc -> Just (clas, tys)
        _ -> Nothing

classMethodTy :: Id -> Type
-- Takes a class selector op :: forall a. C a => meth_ty
-- and returns the type of its method, meth_ty
-- The selector can be a superclass selector, in which case
-- you get back a superclass
classMethodTy sel_id
  = funResultTy $        -- meth_ty
    dropForAlls $        -- C a => meth_ty
    varType sel_id        -- forall a. C n => meth_ty

classMethodInstTy :: Id -> [Type] -> Type
-- Takes a class selector op :: forall a b. C a b => meth_ty
-- and the types [ty1, ty2] at which it is instantiated,
-- returns the instantiated type of its method, meth_ty[t1/a,t2/b]
-- The selector can be a superclass selector, in which case
-- you get back a superclass
classMethodInstTy sel_id arg_tys
  = funResultTy $
    piResultTys (varType sel_id) arg_tys

{- Note [Type determines value]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Only specialise on non-impicit-parameter predicates, because these
are the ones whose *type* determines their *value*.  In particular,
with implicit params, the type args *don't* say what the value of the
implicit param is!  See #7101.

So we treat implicit params just like ordinary arguments for the
purposes of specialisation.  Note that we still want to specialise
functions with implicit params if they have *other* dicts which are
class params; see #17930.
-}

-- --------------------- Equality predicates ---------------------------------

-- | A choice of equality relation. This is separate from the type 'Role'
-- because 'Phantom' does not define a (non-trivial) equality relation.
data EqRel = NomEq | ReprEq
  deriving (Eq, Ord)

instance Outputable EqRel where
  ppr NomEq  = text "nominal equality"
  ppr ReprEq = text "representational equality"

eqRelRole :: EqRel -> Role
eqRelRole NomEq  = Nominal
eqRelRole ReprEq = Representational

-- | Creates a primitive nominal type equality predicate.
--      t1 ~# t2
-- Invariant: the types are not Coercions
mkNomEqPred :: Type -> Type -> Type
mkNomEqPred ty1 ty2
  = mkTyConApp eqPrimTyCon [k1, k2, ty1, ty2]
  where
    k1 = typeKind ty1
    k2 = typeKind ty2

-- | Creates a primitive representational type equality predicate.
--      t1 ~R# t2
-- Invariant: the types are not Coercions
mkReprEqPred :: Type -> Type -> Type
mkReprEqPred ty1  ty2
  = mkTyConApp eqReprPrimTyCon [k1, k2, ty1, ty2]
  where
    k1 = typeKind ty1
    k2 = typeKind ty2

-- | Makes a lifted equality predicate at the given role
mkEqPred :: EqRel -> Type -> Type -> PredType
mkEqPred NomEq  = mkNomEqPred
mkEqPred ReprEq = mkReprEqPred

-- | Makes a lifted equality predicate at the given role
mkEqPredRole :: Role -> Type -> Type -> PredType
mkEqPredRole Nominal          = mkNomEqPred
mkEqPredRole Representational = mkReprEqPred
mkEqPredRole Phantom          = panic "mkEqPred phantom"

getEqPredTys :: PredType -> (Type, Type)
getEqPredTys ty
  = case splitTyConApp_maybe ty of
      Just (tc, [_, _, ty1, ty2])
        |  tc `hasKey` eqPrimTyConKey
        || tc `hasKey` eqReprPrimTyConKey
        -> (ty1, ty2)
      _ -> pprPanic "getEqPredTys" (ppr ty)

getEqPredTys_maybe :: PredType -> Maybe (Role, Type, Type)
getEqPredTys_maybe ty
  = case splitTyConApp_maybe ty of
      Just (tc, [_, _, ty1, ty2])
        | tc `hasKey` eqPrimTyConKey     -> Just (Nominal, ty1, ty2)
        | tc `hasKey` eqReprPrimTyConKey -> Just (Representational, ty1, ty2)
      _ -> Nothing

getEqPredRole :: PredType -> Role
-- Precondition: the PredType is (s ~#N t) or (s ~#R t)
getEqPredRole ty = eqRelRole (predTypeEqRel ty)

-- | Get the equality relation relevant for a pred type
-- Returns NomEq for dictionary predicates, etc
predTypeEqRel :: PredType -> EqRel
predTypeEqRel ty
  | isReprEqPred ty = ReprEq
  | otherwise       = NomEq

{-------------------------------------------
Predicates on PredType
--------------------------------------------}

{-
Note [Evidence for quantified constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The superclass mechanism in GHC.Tc.Solver.Dict.makeSuperClasses risks
taking a quantified constraint like
   (forall a. C a => a ~ b)
and generate superclass evidence
   (forall a. C a => a ~# b)

This is a funny thing: neither isPredTy nor isCoVarType are true
of it.  So we are careful not to generate it in the first place:
see Note [Equality superclasses in quantified constraints]
in GHC.Tc.Solver.Dict.
-}

isPredTy :: HasDebugCallStack => Type -> Bool
-- Precondition: expects a type that classifies values
-- See Note [Types for coercions, predicates, and evidence] in GHC.Core.TyCo.Rep
-- Returns True for types of kind (CONSTRAINT _), False for ones of kind (TYPE _)
isPredTy ty = case typeTypeOrConstraint ty of
                  TypeLike       -> False
                  ConstraintLike -> True

-- | Does this type classify a core (unlifted) Coercion?
-- At either role nominal or representational
--    (t1 ~# t2) or (t1 ~R# t2)
-- See Note [Types for coercions, predicates, and evidence] in "GHC.Core.TyCo.Rep"
isCoVarType :: Type -> Bool
  -- ToDo: should we check saturation?
isCoVarType ty = isEqPred ty

isEvVarType :: Type -> Bool
-- True of (a) predicates, of kind Constraint, such as (Eq t), and (s ~ t)
--         (b) coercion types, such as (s ~# t) or (s ~R# t)
-- See Note [Types for coercions, predicates, and evidence] in GHC.Core.TyCo.Rep
-- See Note [Evidence for quantified constraints]
isEvVarType ty = isCoVarType ty || isPredTy ty

isEqPred :: PredType -> Bool
-- True of (s ~# t) (s ~R# t)
-- NB: but NOT true of (s ~ t) or (s ~~ t) or (Coecible s t)
isEqPred ty
  | Just tc <- tyConAppTyCon_maybe ty
  = tc `hasKey` eqPrimTyConKey || tc `hasKey` eqReprPrimTyConKey
  | otherwise
  = False

isReprEqPred :: PredType -> Bool
-- True of (s ~R# t)
isReprEqPred ty
  | Just tc <- tyConAppTyCon_maybe ty
  = tc `hasKey` eqReprPrimTyConKey
  | otherwise
  = False

isClassPred :: PredType -> Bool
isClassPred ty = case tyConAppTyCon_maybe ty of
    Just tc -> isClassTyCon tc
    _       -> False

isEqClassPred :: PredType -> Bool
isEqClassPred ty  -- True of (s ~ t) and (s ~~ t)
                  -- ToDo: should we check saturation?
  | Just tc <- tyConAppTyCon_maybe ty
  , Just cls <- tyConClass_maybe tc
  = isEqualityClass cls
  | otherwise
  = False

isEqualityClass :: Class -> Bool
-- True of (~), (~~), and Coercible
-- These all have a single primitive-equality superclass, either (~N# or ~R#)
isEqualityClass cls
  = cls `hasKey` heqTyConKey
    || cls `hasKey` eqTyConKey
    || cls `hasKey` coercibleTyConKey

isCTupleClass :: Class -> Bool
isCTupleClass cls = isTupleTyCon (classTyCon cls)

{- *********************************************************************
*                                                                      *
              Implicit parameters
*                                                                      *
********************************************************************* -}

isIPTyCon :: TyCon -> Bool
isIPTyCon tc = tc `hasKey` ipClassKey
  -- Class and its corresponding TyCon have the same Unique

isIPClass :: Class -> Bool
isIPClass cls = cls `hasKey` ipClassKey

-- | Decomposes a predicate if it is an implicit parameter. Does not look in
-- superclasses. See also [Local implicit parameters].
isIPPred_maybe :: Class -> [Type] -> Maybe (Type, Type)
isIPPred_maybe cls tys
  | isIPClass cls
  , [t1,t2] <- tys
  = Just (t1,t2)
  | otherwise
  = Nothing

-- --------------------- ExceptionContext predicates --------------------------

-- | Is a 'PredType' an @ExceptionContext@ implicit parameter?
--
-- If so, return the name of the parameter.
isExceptionContextPred :: Class -> [Type] -> Maybe FastString
isExceptionContextPred cls tys
  | [ty1, ty2] <- tys
  , isIPClass cls
  , isExceptionContextTy ty2
  = isStrLitTy ty1
  | otherwise
  = Nothing

-- | Is a type a 'CallStack'?
isExceptionContextTy :: Type -> Bool
isExceptionContextTy ty
  | Just tc <- tyConAppTyCon_maybe ty
  = tc `hasKey` exceptionContextTyConKey
  | otherwise
  = False

-- --------------------- CallStack predicates ---------------------------------

isCallStackPredTy :: Type -> Bool
-- True of HasCallStack, or IP "blah" CallStack
isCallStackPredTy ty
  | Just (tc, tys) <- splitTyConApp_maybe ty
  , Just cls <- tyConClass_maybe tc
  , Just {} <- isCallStackPred cls tys
  = True
  | otherwise
  = False

-- | Is a 'PredType' a 'CallStack' implicit parameter?
--
-- If so, return the name of the parameter.
isCallStackPred :: Class -> [Type] -> Maybe FastString
isCallStackPred cls tys
  | [ty1, ty2] <- tys
  , isIPClass cls
  , isCallStackTy ty2
  = isStrLitTy ty1
  | otherwise
  = Nothing

-- | Is a type a 'CallStack'?
isCallStackTy :: Type -> Bool
isCallStackTy ty
  | Just tc <- tyConAppTyCon_maybe ty
  = tc `hasKey` callStackTyConKey
  | otherwise
  = False

-- --------------------- isIPLike and mentionsIP  --------------------------
--                 See Note [Local implicit parameters]

isIPLikePred :: Type -> Bool
-- Is `pred`, or any of its superclasses, an implicit parameter?
-- See Note [Local implicit parameters]
isIPLikePred pred = mentions_ip_pred initIPRecTc Nothing pred

mentionsIP :: Type -> Class -> [Type] -> Bool
-- Is (cls tys) an implicit parameter with key `str_ty`, or
-- is any of its superclasses such at thing.
-- See Note [Local implicit parameters]
mentionsIP str_ty cls tys = mentions_ip initIPRecTc (Just str_ty) cls tys

mentions_ip :: RecTcChecker -> Maybe Type -> Class -> [Type] -> Bool
mentions_ip rec_clss mb_str_ty cls tys
  | Just (str_ty', _) <- isIPPred_maybe cls tys
  = case mb_str_ty of
       Nothing -> True
       Just str_ty -> str_ty `eqType` str_ty'
  | otherwise
  = or [ mentions_ip_pred rec_clss mb_str_ty (classMethodInstTy sc_sel_id tys)
       | sc_sel_id <- classSCSelIds cls ]

mentions_ip_pred :: RecTcChecker -> Maybe Type -> Type -> Bool
mentions_ip_pred  rec_clss mb_str_ty ty
  | Just (cls, tys) <- getClassPredTys_maybe ty
  , let tc = classTyCon cls
  , Just rec_clss' <- if isTupleTyCon tc then Just rec_clss
                      else checkRecTc rec_clss tc
  = mentions_ip rec_clss' mb_str_ty cls tys
  | otherwise
  = False -- Includes things like (D []) where D is
          -- a Constraint-ranged family; #7785

initIPRecTc :: RecTcChecker
initIPRecTc = setRecTcMaxBound 1 initRecTc

{- Note [Local implicit parameters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also wrinkle (SIP1) in Note [Shadowing of implicit parameters] in
GHC.Tc.Solver.Dict.

The function isIPLikePred tells if this predicate, or any of its
superclasses, is an implicit parameter.

Why are implicit parameters special?  Unlike normal classes, we can
have local instances for implicit parameters, in the form of
   let ?x = True in ...
So in various places we must be careful not to assume that any value
of the right type will do; we must carefully look for the innermost binding.
So isIPLikePred checks whether this is an implicit parameter, or has
a superclass that is an implicit parameter.

Several wrinkles

* We must be careful with superclasses, as #18649 showed.  Haskell
  doesn't allow an implicit parameter as a superclass
    class (?x::a) => C a where ...
  but with a constraint tuple we might have
     (% Eq a, ?x::Int %)
  and /its/ superclasses, namely (Eq a) and (?x::Int), /do/ include an
  implicit parameter.

  With ConstraintKinds this can apply to /any/ class, e.g.
     class sc => C sc where ...
  Then (C (?x::Int)) has (?x::Int) as a superclass.  So we must
  instantiate and check each superclass, one by one, in
  hasIPSuperClasses.

* With -XUndecidableSuperClasses, the superclass hunt can go on forever,
  so we need a RecTcChecker to cut it off.

* Another apparent additional complexity involves type families. For
  example, consider
         type family D (v::*->*) :: Constraint
         type instance D [] = ()
         f :: D v => v Char -> Int
  If we see a call (f "foo"), we'll pass a "dictionary"
    () |> (g :: () ~ D [])
  and it's good to specialise f at this dictionary.

So the question is: can an implicit parameter "hide inside" a
type-family constraint like (D a).  Well, no.  We don't allow
        type instance D Maybe = ?x:Int
Hence the umbrella 'otherwise' case in is_ip_like_pred.  See #7785.

Small worries (Sept 20):
* I don't see what stops us having that 'type instance'. Indeed I
  think nothing does.
* I'm a little concerned about type variables; such a variable might
  be instantiated to an implicit parameter.  I don't think this
  matters in the cases for which isIPLikePred is used, and it's pretty
  obscure anyway.
* The superclass hunt stops when it encounters the same class again,
  but in principle we could have the same class, differently instantiated,
  and the second time it could have an implicit parameter
I'm going to treat these as problems for another day. They are all exotic.  -}

{- *********************************************************************
*                                                                      *
              Evidence variables
*                                                                      *
********************************************************************* -}

isEvVar :: Var -> Bool
isEvVar var = isEvVarType (varType var)

isDictId :: Id -> Bool
isDictId id = isDictTy (varType id)


{- *********************************************************************
*                                                                      *
                 scopedSort

       This function lives here becuase it uses isEvVar
*                                                                      *
********************************************************************* -}

{- Note [ScopedSort]
~~~~~~~~~~~~~~~~~~~~
Consider

  foo :: Proxy a -> Proxy (b :: k) -> Proxy (a :: k2) -> ()

This function type is implicitly generalised over [a, b, k, k2]. These
variables will be Specified; that is, they will be available for visible
type application. This is because they are written in the type signature
by the user.

However, we must ask: what order will they appear in? In cases without
dependency, this is easy: we just use the lexical left-to-right ordering
of first occurrence. With dependency, we cannot get off the hook so
easily.

We thus state:

 * These variables appear in the order as given by ScopedSort, where
   the input to ScopedSort is the left-to-right order of first occurrence.

Note that this applies only to *implicit* quantification, without a
`forall`. If the user writes a `forall`, then we just use the order given.

ScopedSort is defined thusly (as proposed in #15743):
  * Work left-to-right through the input list, with a cursor.
  * If variable v at the cursor is depended on by any earlier variable w,
    move v immediately before the leftmost such w.

INVARIANT: The prefix of variables before the cursor form a valid telescope.

Note that ScopedSort makes sense only after type inference is done and all
types/kinds are fully settled and zonked.

-}

-- | Do a topological sort on a list of tyvars,
--   so that binders occur before occurrences
-- E.g. given  [ a::k, k::*, b::k ]
-- it'll return a well-scoped list [ k::*, a::k, b::k ]
--
-- This is a deterministic sorting operation
-- (that is, doesn't depend on Uniques).
--
-- It is also meant to be stable: that is, variables should not
-- be reordered unnecessarily. This is specified in Note [ScopedSort]
-- See also Note [Ordering of implicit variables] in "GHC.Rename.HsType"

scopedSort :: [Var] -> [Var]
scopedSort = go [] []
  where
    go :: [Var] -- already sorted, in reverse order
       -> [TyCoVarSet] -- each set contains all the variables which must be placed
                       -- before the tv corresponding to the set; they are accumulations
                       -- of the fvs in the sorted Var's types

                       -- This list is in 1-to-1 correspondence with the sorted Vars
                       -- INVARIANT:
                       --   all (\tl -> all (`subVarSet` head tl) (tail tl)) (tails fv_list)
                       -- That is, each set in the list is a superset of all later sets.

       -> [Var] -- yet to be sorted
       -> [Var]
    go acc _fv_list [] = reverse acc
    go acc  fv_list (tv:tvs)
      = go acc' fv_list' tvs
      where
        (acc', fv_list') = insert tv acc fv_list

    insert :: Var           -- var to insert
           -> [Var]         -- sorted list, in reverse order
           -> [TyCoVarSet]  -- list of fvs, as above
           -> ([Var], [TyCoVarSet])   -- augmented lists
    -- Generally we put the new Var at the front of the accumulating list
    -- (leading to a stable sort) unless there is are reason to put it later.
    insert v []     []         = ([v], [tyCoVarsOfType (varType v)])
    insert v (a:as) (fvs:fvss)
      | (isTyVar v && isId a) ||          -- TyVars precede Ids
        (isEvVar v && isId a && not (isEvVar a)) || -- DictIds precede non-DictIds
        (v `elemVarSet` fvs)
          -- (a) put Ids after TyVars, and (b) respect dependencies
      , (as', fvss') <- insert v as fvss
      = (a:as', fvs `unionVarSet` fv_v : fvss')

      | otherwise  -- Put `v` at the front
      = (v:a:as, fvs `unionVarSet` fv_v : fvs : fvss)
      where
        fv_v = tyCoVarsOfType (varType v)

       -- lists not in correspondence
    insert _ _ _ = panic "scopedSort"

-- | Get the free vars of a type in scoped order
tyCoVarsOfTypeWellScoped :: Type -> [TyVar]
tyCoVarsOfTypeWellScoped = scopedSort . tyCoVarsOfTypeList

-- | Get the free vars of types in scoped order
tyCoVarsOfTypesWellScoped :: [Type] -> [TyVar]
tyCoVarsOfTypesWellScoped = scopedSort . tyCoVarsOfTypesList


{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}

module GHC.Tc.Solver.Equality(
     solveCanonicalEquality, solveNonCanonicalEquality
  ) where


import GHC.Prelude

import GHC.Tc.Solver.Rewrite
import GHC.Tc.Solver.Monad
import GHC.Tc.Solver.Dict( matchLocalInst, chooseInstance )
import GHC.Tc.Solver.InertSet
import GHC.Tc.Solver.Types( findFunEqsByTyCon )
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.Unify
import GHC.Tc.Utils.TcType
import GHC.Tc.Instance.Family ( tcTopNormaliseNewTypeTF_maybe )
import GHC.Tc.Instance.FunDeps( FunDepEqn(..) )

import GHC.Core.Type
import GHC.Core.Predicate
import GHC.Core.Class
import GHC.Core.DataCon ( dataConName )
import GHC.Core.TyCon
import GHC.Core.TyCo.Rep   -- cleverly decomposes types, good for completeness checking
import GHC.Core.Coercion
import GHC.Core.Coercion.Axiom
import GHC.Core.Reduction
import GHC.Core.Unify( tcUnifyTyWithTFs )
import GHC.Core.InstEnv ( Coherence(..) )
import GHC.Core.FamInstEnv ( FamInstEnvs, FamInst(..), apartnessCheck
                           , lookupFamInstEnvByTyCon )
import GHC.Core

import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Types.Var.Set( anyVarSet )
import GHC.Types.Name.Reader
import GHC.Types.Basic

import GHC.Builtin.Types ( anyTypeOfKind )

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Utils.Misc
import GHC.Utils.Monad
import GHC.Utils.Constants( debugIsOn )

import GHC.Data.Pair
import GHC.Data.Bag
import Control.Monad
import Data.Maybe ( isJust, isNothing )
import Data.List  ( zip4 )

import qualified Data.Semigroup as S
import Data.Bifunctor ( bimap )


{- *********************************************************************
*                                                                      *
*        Equalities
*                                                                      *
************************************************************************

Note [Canonicalising equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In order to canonicalise an equality, we look at the structure of the
two types at hand, looking for similarities. A difficulty is that the
types may look dissimilar before rewriting but similar after rewriting.
However, we don't just want to jump in and rewrite right away, because
this might be wasted effort. So, after looking for similarities and failing,
we rewrite and then try again. Of course, we don't want to loop, so we
track whether or not we've already rewritten.

It is conceivable to do a better job at tracking whether or not a type
is rewritten, but this is left as future work. (Mar '15)

Note [Decomposing FunTy]
~~~~~~~~~~~~~~~~~~~~~~~~
can_eq_nc' may attempt to decompose a FunTy that is un-zonked.  This
means that we may very well have a FunTy containing a type of some
unknown kind. For instance, we may have,

    FunTy (a :: k) Int

Where k is a unification variable. So the calls to splitRuntimeRep_maybe may
fail (returning Nothing).  In that case we'll fall through, zonk, and try again.
Zonking should fill the variable k, meaning that decomposition will succeed the
second time around.

Also note that we require the FunTyFlag to match.  This will stop
us decomposing
   (Int -> Bool)  ~  (Show a => blah)
It's as if we treat (->) and (=>) as different type constructors, which
indeed they are!
-}

solveCanonicalEquality :: EqCt -> TcS (StopOrContinue Ct)
solveCanonicalEquality (EqCt { eq_ev = ev, eq_eq_rel = eq_rel
                             , eq_lhs = lhs, eq_rhs = rhs })
  = solveNonCanonicalEquality ev eq_rel (canEqLHSType lhs) rhs

solveNonCanonicalEquality :: CtEvidence -> EqRel -> Type -> Type -> TcS (StopOrContinue Ct)
solveNonCanonicalEquality ev eq_rel ty1 ty2
  = do { result <- zonk_eq_types ty1 ty2
       ; case result of
           Right ty              -> canEqReflexive ev eq_rel ty
           Left (Pair ty1' ty2') -> can_eq_nc False ev' eq_rel ty1' ty1' ty2' ty2'
             where
               ev' | debugIsOn = setCtEvPredType ev $
                                 mkPrimEqPredRole (eqRelRole eq_rel) ty1' ty2'
                   | otherwise = ev
                   -- ev': satisfy the precondition of can_eq_nc
       }

can_eq_nc
   :: Bool            -- True => both types are rewritten
   -> CtEvidence
   -> EqRel
   -> Type -> Type    -- LHS, after and before type-synonym expansion, resp
   -> Type -> Type    -- RHS, after and before type-synonym expansion, resp
   -> TcS (StopOrContinue Ct)
-- Precondition: in DEBUG mode, the `ctev_pred` of `ev` is (ps_ty1 ~# ps_ty2),
--               without zonking
-- This precondition is needed (only in DEBUG) to satisfy the assertions
--   in mkSelCo, called in canDecomposableTyConAppOK and canDecomposableFunTy

can_eq_nc rewritten ev eq_rel ty1 ps_ty1 ty2 ps_ty2
  = do { traceTcS "can_eq_nc" $
         vcat [ ppr rewritten, ppr ev, ppr eq_rel, ppr ty1, ppr ps_ty1, ppr ty2, ppr ps_ty2 ]
       ; rdr_env <- getGlobalRdrEnvTcS
       ; fam_insts <- getFamInstEnvs
       ; can_eq_nc' rewritten rdr_env fam_insts ev eq_rel ty1 ps_ty1 ty2 ps_ty2 }

can_eq_nc'
   :: Bool           -- True => both input types are rewritten
   -> GlobalRdrEnv   -- needed to see which newtypes are in scope
   -> FamInstEnvs    -- needed to unwrap data instances
   -> CtEvidence
   -> EqRel
   -> Type -> Type    -- LHS, after and before type-synonym expansion, resp
   -> Type -> Type    -- RHS, after and before type-synonym expansion, resp
   -> TcS (StopOrContinue Ct)

-- See Note [Comparing nullary type synonyms] in GHC.Core.Type.
can_eq_nc' _flat _rdr_env _envs ev eq_rel ty1@(TyConApp tc1 []) _ps_ty1 (TyConApp tc2 []) _ps_ty2
  | tc1 == tc2
  = canEqReflexive ev eq_rel ty1

-- Expand synonyms first; see Note [Type synonyms and canonicalization]
can_eq_nc' rewritten rdr_env envs ev eq_rel ty1 ps_ty1 ty2 ps_ty2
  | Just ty1' <- coreView ty1 = can_eq_nc' rewritten rdr_env envs ev eq_rel ty1' ps_ty1 ty2  ps_ty2
  | Just ty2' <- coreView ty2 = can_eq_nc' rewritten rdr_env envs ev eq_rel ty1  ps_ty1 ty2' ps_ty2

-- need to check for reflexivity in the ReprEq case.
-- See Note [Eager reflexivity check]
-- Check only when rewritten because the zonk_eq_types check in canEqNC takes
-- care of the non-rewritten case.
can_eq_nc' True _rdr_env _envs ev ReprEq ty1 _ ty2 _
  | ty1 `tcEqType` ty2
  = canEqReflexive ev ReprEq ty1

-- When working with ReprEq, unwrap newtypes.
-- See Note [Unwrap newtypes first]
-- This must be above the TyVarTy case, in order to guarantee (TyEq:N)
can_eq_nc' _rewritten rdr_env envs ev eq_rel ty1 ps_ty1 ty2 ps_ty2
  | ReprEq <- eq_rel
  , Just stuff1 <- tcTopNormaliseNewTypeTF_maybe envs rdr_env ty1
  = can_eq_newtype_nc ev NotSwapped ty1 stuff1 ty2 ps_ty2

  | ReprEq <- eq_rel
  , Just stuff2 <- tcTopNormaliseNewTypeTF_maybe envs rdr_env ty2
  = can_eq_newtype_nc ev IsSwapped ty2 stuff2 ty1 ps_ty1

-- Then, get rid of casts
can_eq_nc' rewritten _rdr_env _envs ev eq_rel (CastTy ty1 co1) _ ty2 ps_ty2
  | isNothing (canEqLHS_maybe ty2)  -- See (3) in Note [Equalities with incompatible kinds]
  = canEqCast rewritten ev eq_rel NotSwapped ty1 co1 ty2 ps_ty2
can_eq_nc' rewritten _rdr_env _envs ev eq_rel ty1 ps_ty1 (CastTy ty2 co2) _
  | isNothing (canEqLHS_maybe ty1)  -- See (3) in Note [Equalities with incompatible kinds]
  = canEqCast rewritten ev eq_rel IsSwapped ty2 co2 ty1 ps_ty1

----------------------
-- Otherwise try to decompose
----------------------

-- Literals
can_eq_nc' _rewritten _rdr_env _envs ev eq_rel ty1@(LitTy l1) _ (LitTy l2) _
 | l1 == l2
  = do { setEvBindIfWanted ev IsCoherent (evCoercion $ mkReflCo (eqRelRole eq_rel) ty1)
       ; stopWith ev "Equal LitTy" }

-- Decompose FunTy: (s -> t) and (c => t)
-- NB: don't decompose (Int -> blah) ~ (Show a => blah)
can_eq_nc' _rewritten _rdr_env _envs ev eq_rel
           (FunTy { ft_mult = am1, ft_af = af1, ft_arg = ty1a, ft_res = ty1b }) _ps_ty1
           (FunTy { ft_mult = am2, ft_af = af2, ft_arg = ty2a, ft_res = ty2b }) _ps_ty2
  | af1 == af2  -- See Note [Decomposing FunTy]
  = canDecomposableFunTy ev eq_rel af1 (am1,ty1a,ty1b) (am2,ty2a,ty2b)

-- Decompose type constructor applications
-- NB: we have expanded type synonyms already
can_eq_nc' _rewritten _rdr_env _envs ev eq_rel ty1 _ ty2 _
  | Just (tc1, tys1) <- tcSplitTyConApp_maybe ty1
  , Just (tc2, tys2) <- tcSplitTyConApp_maybe ty2
   -- we want to catch e.g. Maybe Int ~ (Int -> Int) here for better
   -- error messages rather than decomposing into AppTys;
   -- hence no direct match on TyConApp
  , not (isTypeFamilyTyCon tc1)
  , not (isTypeFamilyTyCon tc2)
  = canTyConApp ev eq_rel tc1 tys1 tc2 tys2

can_eq_nc' _rewritten _rdr_env _envs ev eq_rel
           s1@(ForAllTy (Bndr _ vis1) _) _
           s2@(ForAllTy (Bndr _ vis2) _) _
  | vis1 `eqForAllVis` vis2 -- Note [ForAllTy and type equality]
  = can_eq_nc_forall ev eq_rel s1 s2

-- See Note [Canonicalising type applications] about why we require rewritten types
-- Use tcSplitAppTy, not matching on AppTy, to catch oversaturated type families
-- NB: Only decompose AppTy for nominal equality.
--     See Note [Decomposing AppTy equalities]
can_eq_nc' True _rdr_env _envs ev NomEq ty1 _ ty2 _
  | Just (t1, s1) <- tcSplitAppTy_maybe ty1
  , Just (t2, s2) <- tcSplitAppTy_maybe ty2
  = can_eq_app ev t1 s1 t2 s2

-------------------
-- Can't decompose.
-------------------

-- No similarity in type structure detected. Rewrite and try again.
can_eq_nc' False rdr_env envs ev eq_rel _ ps_ty1 _ ps_ty2
  = -- Rewrite the two types and try again
    do { (redn1@(Reduction _ xi1), rewriters1) <- rewrite ev ps_ty1
       ; (redn2@(Reduction _ xi2), rewriters2) <- rewrite ev ps_ty2
       ; new_ev <- rewriteEqEvidence (rewriters1 S.<> rewriters2) ev NotSwapped redn1 redn2
       ; traceTcS "can_eq_nc: go round again" (ppr new_ev $$ ppr xi1 $$ ppr xi2)
       ; can_eq_nc' True rdr_env envs new_ev eq_rel xi1 xi1 xi2 xi2 }

----------------------------
-- Look for a canonical LHS.
-- Only rewritten types end up below here.
----------------------------

-- NB: pattern match on True: we want only rewritten types sent to canEqLHS
-- This means we've rewritten any variables and reduced any type family redexes
-- See also Note [No top-level newtypes on RHS of representational equalities]
can_eq_nc' True _rdr_env _envs ev eq_rel ty1 ps_ty1 ty2 ps_ty2
  | Just can_eq_lhs1 <- canEqLHS_maybe ty1
  = canEqCanLHS ev eq_rel NotSwapped can_eq_lhs1 ps_ty1 ty2 ps_ty2

  | Just can_eq_lhs2 <- canEqLHS_maybe ty2
  = canEqCanLHS ev eq_rel IsSwapped can_eq_lhs2 ps_ty2 ty1 ps_ty1

     -- If the type is TyConApp tc1 args1, then args1 really can't be less
     -- than tyConArity tc1. It could be *more* than tyConArity, but then we
     -- should have handled the case as an AppTy. That case only fires if
     -- _both_ sides of the equality are AppTy-like... but if one side is
     -- AppTy-like and the other isn't (and it also isn't a variable or
     -- saturated type family application, both of which are handled by
     -- can_eq_nc'), we're in a failure mode and can just fall through.

----------------------------
-- Fall-through. Give up.
----------------------------

-- We've rewritten and the types don't match. Give up.
can_eq_nc' True _rdr_env _envs ev eq_rel _ ps_ty1 _ ps_ty2
  = do { traceTcS "can_eq_nc' catch-all case" (ppr ps_ty1 $$ ppr ps_ty2)
       ; case eq_rel of -- See Note [Unsolved equalities]
            ReprEq -> solveIrredEquality ReprEqReason ev
            NomEq  -> solveIrredEquality ShapeMismatchReason ev }
          -- No need to call canEqFailure/canEqHardFailure because they
          -- rewrite, and the types involved here are already rewritten


{- Note [Unsolved equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have an unsolved equality like
  (a b ~R# Int)
that is not necessarily insoluble!  Maybe 'a' will turn out to be a newtype.
So we want to make it a potentially-soluble Irred not an insoluble one.
Missing this point is what caused #15431
-}

---------------------------------
can_eq_nc_forall :: CtEvidence -> EqRel
                 -> Type -> Type    -- LHS and RHS
                 -> TcS (StopOrContinue Ct)
-- (forall as. phi1) ~ (forall bs. phi2)
-- Check for length match of as, bs
-- Then build an implication constraint: forall as. phi1 ~ phi2[as/bs]
-- But remember also to unify the kinds of as and bs
--  (this is the 'go' loop), and actually substitute phi2[as |> cos / bs]
-- Remember also that we might have forall z (a:z). blah
--  so we must proceed one binder at a time (#13879)

can_eq_nc_forall ev eq_rel s1 s2
 | CtWanted { ctev_loc = loc, ctev_dest = orig_dest, ctev_rewriters = rewriters } <- ev
 = do { let free_tvs       = tyCoVarsOfTypes [s1,s2]
            (bndrs1, phi1) = tcSplitForAllTyVarBinders s1
            (bndrs2, phi2) = tcSplitForAllTyVarBinders s2
      ; if not (equalLength bndrs1 bndrs2)
        then do { traceTcS "Forall failure" $
                     vcat [ ppr s1, ppr s2, ppr bndrs1, ppr bndrs2
                          , ppr (binderFlags bndrs1)
                          , ppr (binderFlags bndrs2) ]
                ; canEqHardFailure ev s1 s2 }
        else
   do { traceTcS "Creating implication for polytype equality" $ ppr ev
      ; let empty_subst1 = mkEmptySubst $ mkInScopeSet free_tvs
      ; skol_info <- mkSkolemInfo (UnifyForAllSkol phi1)
      ; (subst1, skol_tvs) <- tcInstSkolTyVarsX skol_info empty_subst1 $
                              binderVars bndrs1

      ; let phi1' = substTy subst1 phi1

            -- Unify the kinds, extend the substitution
            go :: [TcTyVar] -> Subst -> [TyVarBinder]
               -> TcS (TcCoercion, Cts)
            go (skol_tv:skol_tvs) subst (bndr2:bndrs2)
              = do { let tv2 = binderVar bndr2
                   ; (kind_co, wanteds1) <- unify loc rewriters Nominal (tyVarKind skol_tv)
                                                  (substTy subst (tyVarKind tv2))
                   ; let subst' = extendTvSubstAndInScope subst tv2
                                       (mkCastTy (mkTyVarTy skol_tv) kind_co)
                         -- skol_tv is already in the in-scope set, but the
                         -- free vars of kind_co are not; hence "...AndInScope"
                   ; (co, wanteds2) <- go skol_tvs subst' bndrs2
                   ; return ( mkForAllCo skol_tv kind_co co
                            , wanteds1 `unionBags` wanteds2 ) }

            -- Done: unify phi1 ~ phi2
            go [] subst bndrs2
              = assert (null bndrs2) $
                unify loc rewriters (eqRelRole eq_rel) phi1' (substTyUnchecked subst phi2)

            go _ _ _ = panic "cna_eq_nc_forall"  -- case (s:ss) []

            empty_subst2 = mkEmptySubst (getSubstInScope subst1)

      ; (lvl, (all_co, wanteds)) <- pushLevelNoWorkList (ppr skol_info) $
                                    go skol_tvs empty_subst2 bndrs2
      ; emitTvImplicationTcS lvl (getSkolemInfo skol_info) skol_tvs wanteds

      ; setWantedEq orig_dest all_co
      ; stopWith ev "Deferred polytype equality" } }

 | otherwise
 = do { traceTcS "Omitting decomposition of given polytype equality" $
        pprEq s1 s2    -- See Note [Do not decompose Given polytype equalities]
      ; stopWith ev "Discard given polytype equality" }

 where
    unify :: CtLoc -> RewriterSet -> Role -> TcType -> TcType -> TcS (TcCoercion, Cts)
    -- This version returns the wanted constraint rather
    -- than putting it in the work list
    unify loc rewriters role ty1 ty2
      | ty1 `tcEqType` ty2
      = return (mkReflCo role ty1, emptyBag)
      | otherwise
      = do { (wanted, co) <- newWantedEq loc rewriters role ty1 ty2
           ; return (co, unitBag (mkNonCanonical wanted)) }

---------------------------------
-- | Compare types for equality, while zonking as necessary. Gives up
-- as soon as it finds that two types are not equal.
-- This is quite handy when some unification has made two
-- types in an inert Wanted to be equal. We can discover the equality without
-- rewriting, which is sometimes very expensive (in the case of type functions).
-- In particular, this function makes a ~20% improvement in test case
-- perf/compiler/T5030.
--
-- Returns either the (partially zonked) types in the case of
-- inequality, or the one type in the case of equality. canEqReflexive is
-- a good next step in the 'Right' case. Returning 'Left' is always safe.
--
-- NB: This does *not* look through type synonyms. In fact, it treats type
-- synonyms as rigid constructors. In the future, it might be convenient
-- to look at only those arguments of type synonyms that actually appear
-- in the synonym RHS. But we're not there yet.
zonk_eq_types :: TcType -> TcType -> TcS (Either (Pair TcType) TcType)
zonk_eq_types = go
  where
    go (TyVarTy tv1) (TyVarTy tv2) = tyvar_tyvar tv1 tv2
    go (TyVarTy tv1) ty2           = tyvar NotSwapped tv1 ty2
    go ty1 (TyVarTy tv2)           = tyvar IsSwapped  tv2 ty1

    -- We handle FunTys explicitly here despite the fact that they could also be
    -- treated as an application. Why? Well, for one it's cheaper to just look
    -- at two types (the argument and result types) than four (the argument,
    -- result, and their RuntimeReps). Also, we haven't completely zonked yet,
    -- so we may run into an unzonked type variable while trying to compute the
    -- RuntimeReps of the argument and result types. This can be observed in
    -- testcase tc269.
    go (FunTy af1 w1 arg1 res1) (FunTy af2 w2 arg2 res2)
      | af1 == af2
      , eqType w1 w2
      = do { res_a <- go arg1 arg2
           ; res_b <- go res1 res2
           ; return $ combine_rev (FunTy af1 w1) res_b res_a }

    go ty1@(FunTy {}) ty2 = bale_out ty1 ty2
    go ty1 ty2@(FunTy {}) = bale_out ty1 ty2

    go ty1 ty2
      | Just (tc1, tys1) <- splitTyConAppNoView_maybe ty1
      , Just (tc2, tys2) <- splitTyConAppNoView_maybe ty2
      = if tc1 == tc2 && tys1 `equalLength` tys2
          -- Crucial to check for equal-length args, because
          -- we cannot assume that the two args to 'go' have
          -- the same kind.  E.g go (Proxy *      (Maybe Int))
          --                        (Proxy (*->*) Maybe)
          -- We'll call (go (Maybe Int) Maybe)
          -- See #13083
        then tycon tc1 tys1 tys2
        else bale_out ty1 ty2

    go ty1 ty2
      | Just (ty1a, ty1b) <- tcSplitAppTyNoView_maybe ty1
      , Just (ty2a, ty2b) <- tcSplitAppTyNoView_maybe ty2
      = do { res_a <- go ty1a ty2a
           ; res_b <- go ty1b ty2b
           ; return $ combine_rev mkAppTy res_b res_a }

    go ty1@(LitTy lit1) (LitTy lit2)
      | lit1 == lit2
      = return (Right ty1)

    go ty1 ty2 = bale_out ty1 ty2
      -- We don't handle more complex forms here

    bale_out ty1 ty2 = return $ Left (Pair ty1 ty2)

    tyvar :: SwapFlag -> TcTyVar -> TcType
          -> TcS (Either (Pair TcType) TcType)
      -- Try to do as little as possible, as anything we do here is redundant
      -- with rewriting. In particular, no need to zonk kinds. That's why
      -- we don't use the already-defined zonking functions
    tyvar swapped tv ty
      = case tcTyVarDetails tv of
          MetaTv { mtv_ref = ref }
            -> do { cts <- readTcRef ref
                  ; case cts of
                      Flexi        -> give_up
                      Indirect ty' -> do { trace_indirect tv ty'
                                         ; unSwap swapped go ty' ty } }
          _ -> give_up
      where
        give_up = return $ Left $ unSwap swapped Pair (mkTyVarTy tv) ty

    tyvar_tyvar tv1 tv2
      | tv1 == tv2 = return (Right (mkTyVarTy tv1))
      | otherwise  = do { (ty1', progress1) <- quick_zonk tv1
                        ; (ty2', progress2) <- quick_zonk tv2
                        ; if progress1 || progress2
                          then go ty1' ty2'
                          else return $ Left (Pair (TyVarTy tv1) (TyVarTy tv2)) }

    trace_indirect tv ty
       = traceTcS "Following filled tyvar (zonk_eq_types)"
                  (ppr tv <+> equals <+> ppr ty)

    quick_zonk tv = case tcTyVarDetails tv of
      MetaTv { mtv_ref = ref }
        -> do { cts <- readTcRef ref
              ; case cts of
                  Flexi        -> return (TyVarTy tv, False)
                  Indirect ty' -> do { trace_indirect tv ty'
                                     ; return (ty', True) } }
      _ -> return (TyVarTy tv, False)

      -- This happens for type families, too. But recall that failure
      -- here just means to try harder, so it's OK if the type function
      -- isn't injective.
    tycon :: TyCon -> [TcType] -> [TcType]
          -> TcS (Either (Pair TcType) TcType)
    tycon tc tys1 tys2
      = do { results <- zipWithM go tys1 tys2
           ; return $ case combine_results results of
               Left tys  -> Left (mkTyConApp tc <$> tys)
               Right tys -> Right (mkTyConApp tc tys) }

    combine_results :: [Either (Pair TcType) TcType]
                    -> Either (Pair [TcType]) [TcType]
    combine_results = bimap (fmap reverse) reverse .
                      foldl' (combine_rev (:)) (Right [])

      -- combine (in reverse) a new result onto an already-combined result
    combine_rev :: (a -> b -> c)
                -> Either (Pair b) b
                -> Either (Pair a) a
                -> Either (Pair c) c
    combine_rev f (Left list) (Left elt) = Left (f <$> elt     <*> list)
    combine_rev f (Left list) (Right ty) = Left (f <$> pure ty <*> list)
    combine_rev f (Right tys) (Left elt) = Left (f <$> elt     <*> pure tys)
    combine_rev f (Right tys) (Right ty) = Right (f ty tys)

{- Note [Unwrap newtypes first]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also Note [Decomposing newtype equalities]

Consider
  newtype N m a = MkN (m a)
N will get a conservative, Nominal role for its second parameter 'a',
because it appears as an argument to the unknown 'm'. Now consider
  [W] N Maybe a  ~R#  N Maybe b

If we /decompose/, we'll get
  [W] a ~N# b

But if instead we /unwrap/ we'll get
  [W] Maybe a ~R# Maybe b
which in turn gives us
  [W] a ~R# b
which is easier to satisfy.

Conclusion: we must unwrap newtypes before decomposing them. This happens
in `can_eq_newtype_nc`

We did flirt with making the /rewriter/ expand newtypes, rather than
doing it in `can_eq_newtype_nc`.   But with recursive newtypes we want
to be super-careful about expanding!

   newtype A = MkA [A]   -- Recursive!

   f :: A -> [A]
   f = coerce

We have [W] A ~R# [A].  If we rewrite [A], it'll expand to
   [[[[[...]]]]]
and blow the reduction stack.  See Note [Newtypes can blow the stack]
in GHC.Tc.Solver.Rewrite.  But if we expand only the /top level/ of
both sides, we get
   [W] [A] ~R# [A]
which we can, just, solve by reflexivity.

So we simply unwrap, on-demand, at top level, in `can_eq_newtype_nc`.

This is all very delicate. There is a real risk of a loop in the type checker
with recursive newtypes -- but I think we're doomed to do *something*
delicate, as we're really trying to solve for equirecursive type
equality. Bottom line for users: recursive newtypes do not play well with type
inference for representational equality.  See also Section 5.3.1 and 5.3.4 of
"Safe Zero-cost Coercions for Haskell" (JFP 2016).

See also Note [Decomposing newtype equalities].

--- Historical side note ---

We flirted with doing /both/ unwrap-at-top-level /and/ rewrite-deeply;
see #22519.  But that didn't work: see discussion in #22924. Specifically
we got a loop with a minor variation:
   f2 :: a -> [A]
   f2 = coerce

Note [Eager reflexivity check]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have

  newtype X = MkX (Int -> X)

and

  [W] X ~R X

Naively, we would start unwrapping X and end up in a loop. Instead,
we do this eager reflexivity check. This is necessary only for representational
equality because the rewriter technology deals with the similar case
(recursive type families) for nominal equality.

Note that this check does not catch all cases, but it will catch the cases
we're most worried about, types like X above that are actually inhabited.

Here's another place where this reflexivity check is key:
Consider trying to prove (f a) ~R (f a). The AppTys in there can't
be decomposed, because representational equality isn't congruent with respect
to AppTy. So, when canonicalising the equality above, we get stuck and
would normally produce a CIrredCan. However, we really do want to
be able to solve (f a) ~R (f a). So, in the representational case only,
we do a reflexivity check.

(This would be sound in the nominal case, but unnecessary, and I [Richard
E.] am worried that it would slow down the common case.)

 Note [Newtypes can blow the stack]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have

  newtype X = MkX (Int -> X)
  newtype Y = MkY (Int -> Y)

and now wish to prove

  [W] X ~R Y

This Wanted will loop, expanding out the newtypes ever deeper looking
for a solid match or a solid discrepancy. Indeed, there is something
appropriate to this looping, because X and Y *do* have the same representation,
in the limit -- they're both (Fix ((->) Int)). However, no finitely-sized
coercion will ever witness it. This loop won't actually cause GHC to hang,
though, because we check our depth in `can_eq_newtype_nc`.
-}

------------------------
-- | We're able to unwrap a newtype. Update the bits accordingly.
can_eq_newtype_nc :: CtEvidence           -- ^ :: ty1 ~ ty2
                  -> SwapFlag
                  -> TcType                                    -- ^ ty1
                  -> ((Bag GlobalRdrElt, TcCoercion), TcType)  -- ^ :: ty1 ~ ty1'
                  -> TcType               -- ^ ty2
                  -> TcType               -- ^ ty2, with type synonyms
                  -> TcS (StopOrContinue Ct)
can_eq_newtype_nc ev swapped ty1 ((gres, co1), ty1') ty2 ps_ty2
  = do { traceTcS "can_eq_newtype_nc" $
         vcat [ ppr ev, ppr swapped, ppr co1, ppr gres, ppr ty1', ppr ty2 ]

         -- Check for blowing our stack, and increase the depth
         -- See Note [Newtypes can blow the stack]
       ; let loc = ctEvLoc ev
             ev' = ev `setCtEvLoc` bumpCtLocDepth loc
       ; checkReductionDepth loc ty1

         -- Next, we record uses of newtype constructors, since coercing
         -- through newtypes is tantamount to using their constructors.
       ; recordUsedGREs gres

       ; let redn1 = mkReduction co1 ty1'

       ; new_ev <- rewriteEqEvidence emptyRewriterSet ev' swapped
                     redn1
                     (mkReflRedn Representational ps_ty2)
       ; can_eq_nc False new_ev ReprEq ty1' ty1' ty2 ps_ty2 }

---------
-- ^ Decompose a type application.
-- All input types must be rewritten. See Note [Canonicalising type applications]
-- Nominal equality only!
can_eq_app :: CtEvidence       -- :: s1 t1 ~N s2 t2
           -> Xi -> Xi         -- s1 t1
           -> Xi -> Xi         -- s2 t2
           -> TcS (StopOrContinue Ct)

-- AppTys only decompose for nominal equality, so this case just leads
-- to an irreducible constraint; see typecheck/should_compile/T10494
-- See Note [Decomposing AppTy equalities]
can_eq_app ev s1 t1 s2 t2
  | CtWanted { ctev_dest = dest, ctev_rewriters = rewriters } <- ev
  = do { co_s <- unifyWanted rewriters loc Nominal s1 s2
       ; let arg_loc
               | isNextArgVisible s1 = loc
               | otherwise           = updateCtLocOrigin loc toInvisibleOrigin
       ; co_t <- unifyWanted rewriters arg_loc Nominal t1 t2
       ; let co = mkAppCo co_s co_t
       ; setWantedEq dest co
       ; stopWith ev "Decomposed [W] AppTy" }

    -- If there is a ForAll/(->) mismatch, the use of the Left coercion
    -- below is ill-typed, potentially leading to a panic in splitTyConApp
    -- Test case: typecheck/should_run/Typeable1
    -- We could also include this mismatch check above (for W and D), but it's slow
    -- and we'll get a better error message not doing it
  | s1k `mismatches` s2k
  = canEqHardFailure ev (s1 `mkAppTy` t1) (s2 `mkAppTy` t2)

  | CtGiven { ctev_evar = evar } <- ev
  = do { let co   = mkCoVarCo evar
             co_s = mkLRCo CLeft  co
             co_t = mkLRCo CRight co
       ; evar_s <- newGivenEvVar loc ( mkTcEqPredLikeEv ev s1 s2
                                     , evCoercion co_s )
       ; evar_t <- newGivenEvVar loc ( mkTcEqPredLikeEv ev t1 t2
                                     , evCoercion co_t )
       ; emitWorkNC [evar_t]
       ; solveNonCanonicalEquality evar_s NomEq s1 s2 }

  where
    loc = ctEvLoc ev

    s1k = typeKind s1
    s2k = typeKind s2

    k1 `mismatches` k2
      =  isForAllTy k1 && not (isForAllTy k2)
      || not (isForAllTy k1) && isForAllTy k2

-----------------------
-- | Break apart an equality over a casted type
-- looking like   (ty1 |> co1) ~ ty2   (modulo a swap-flag)
canEqCast :: Bool         -- are both types rewritten?
          -> CtEvidence
          -> EqRel
          -> SwapFlag
          -> TcType -> Coercion   -- LHS (res. RHS), ty1 |> co1
          -> TcType -> TcType     -- RHS (res. LHS), ty2 both normal and pretty
          -> TcS (StopOrContinue Ct)
canEqCast rewritten ev eq_rel swapped ty1 co1 ty2 ps_ty2
  = do { traceTcS "Decomposing cast" (vcat [ ppr ev
                                           , ppr ty1 <+> text "|>" <+> ppr co1
                                           , ppr ps_ty2 ])
       ; new_ev <- rewriteEqEvidence emptyRewriterSet ev swapped
                      (mkGReflLeftRedn role ty1 co1)
                      (mkReflRedn role ps_ty2)
       ; can_eq_nc rewritten new_ev eq_rel ty1 ty1 ty2 ps_ty2 }
  where
    role = eqRelRole eq_rel

------------------------
canTyConApp :: CtEvidence -> EqRel
            -> TyCon -> [TcType]
            -> TyCon -> [TcType]
            -> TcS (StopOrContinue Ct)
-- See Note [Decomposing TyConApp equalities]
-- See Note [Decomposing Dependent TyCons and Processing Wanted Equalities]
-- Neither tc1 nor tc2 is a saturated funTyCon, nor a type family
-- But they can be data families.
canTyConApp ev eq_rel tc1 tys1 tc2 tys2
  | tc1 == tc2
  , tys1 `equalLength` tys2
  = do { inerts <- getTcSInerts
       ; if can_decompose inerts
         then canDecomposableTyConAppOK ev eq_rel tc1 tys1 tys2
         else canEqFailure ev eq_rel ty1 ty2 }

  -- See Note [Skolem abstract data] in GHC.Core.Tycon
  | tyConSkolem tc1 || tyConSkolem tc2
  = do { traceTcS "canTyConApp: skolem abstract" (ppr tc1 $$ ppr tc2)
       ; solveIrredEquality AbstractTyConReason ev }

  -- Fail straight away for better error messages
  -- See Note [Use canEqFailure in canDecomposableTyConApp]
  | eq_rel == ReprEq && not (isGenerativeTyCon tc1 Representational &&
                             isGenerativeTyCon tc2 Representational)
  = canEqFailure ev eq_rel ty1 ty2

  | otherwise
  = canEqHardFailure ev ty1 ty2
  where
    -- Reconstruct the types for error messages. This would do
    -- the wrong thing (from a pretty printing point of view)
    -- for functions, because we've lost the FunTyFlag; but
    -- in fact we never call canTyConApp on a saturated FunTyCon
    ty1 = mkTyConApp tc1 tys1
    ty2 = mkTyConApp tc2 tys2

     -- See Note [Decomposing TyConApp equalities]
     -- and Note [Decomposing newtype equalities]
    can_decompose inerts
      =  isInjectiveTyCon tc1 (eqRelRole eq_rel)
      || (assert (eq_rel == ReprEq) $
          -- assert: isInjectiveTyCon is always True for Nominal except
          --   for type synonyms/families, neither of which happen here
          -- Moreover isInjectiveTyCon is True for Representational
          --   for algebraic data types.  So we are down to newtypes
          --   and data families.
          ctEvFlavour ev == Wanted && noGivenNewtypeReprEqs tc1 inerts)
             -- See Note [Decomposing newtype equalities] (EX2)

{-
Note [Use canEqFailure in canDecomposableTyConApp]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We must use canEqFailure, not canEqHardFailure here, because there is
the possibility of success if working with a representational equality.
Here is one case:

  type family TF a where TF Char = Bool
  data family DF a
  newtype instance DF Bool = MkDF Int

Suppose we are canonicalising (Int ~R DF (TF a)), where we don't yet
know `a`. This is *not* a hard failure, because we might soon learn
that `a` is, in fact, Char, and then the equality succeeds.

Here is another case:

  [G] Age ~R Int

where Age's constructor is not in scope. We don't want to report
an "inaccessible code" error in the context of this Given!

For example, see typecheck/should_compile/T10493, repeated here:

  import Data.Ord (Down)  -- no constructor

  foo :: Coercible (Down Int) Int => Down Int -> Int
  foo = coerce

That should compile, but only because we use canEqFailure and not
canEqHardFailure.

Note [Fast path when decomposing TyConApps]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we see (T s1 t1 ~ T s2 t2), then we can just decompose to
  (s1 ~ s2, t1 ~ t2)
and push those back into the work list.  But if
  s1 = K k1    s2 = K k2
then we will just decompose s1~s2, and it might be better to
do so on the spot.  An important special case is where s1=s2,
and we get just Refl.

So canDecomposableTyConAppOK uses unifyWanted etc to short-cut that work.
See also Note [Decomposing Dependent TyCons and Processing Wanted Equalities]

Note [Decomposing TyConApp equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
        [G/W] T ty1 ~r T ty2
Can we decompose it, and replace it by
        [G/W] ty1 ~r' ty2
and if so what role is r'?  (In this Note, all the "~" are primitive
equalities "~#", but I have dropped the noisy "#" symbols.)  Lots of
background in the paper "Safe zero-cost coercions for Haskell".

This Note covers the topic for
  * Datatypes
  * Newtypes
  * Data families
For the rest:
  * Type synonyms: are always expanded
  * Type families: see Note [Decomposing type family applications]
  * AppTy:         see Note [Decomposing AppTy equalities].

---- Roles of the decomposed constraints ----
For a start, the role r' will always be defined like this:
  * If r=N then r' = N
  * If r=R then r' = role of T's first argument

For example:
   data TR a = MkTR a       -- Role of T's first arg is Representational
   data TN a = MkTN (F a)   -- Role of T's first arg is Nominal

The function tyConRolesX :: Role -> TyCon -> [Role] gets the argument
role r' for a TyCon T at role r.  E.g.
   tyConRolesX Nominal          TR = [Nominal]
   tyConRolesX Representational TR = [Representational]

---- Soundness and completeness ----
For Givens, for /soundness/ of decomposition we need, forall ty1,ty2:
    T ty1 ~r T ty2   ===>    ty1 ~r' ty2
Here "===>" means "implies".  That is, given evidence for (co1 : T ty1 ~r T co2)
we can produce evidence for (co2 : ty1 ~r' ty2).  But in the solver we
/replace/ co1 with co2 in the inert set, and we don't want to lose any proofs
thereby. So for /completeness/ of decomposition we also need the reverse:
    ty1 ~r' ty2   ===>    T ty1 ~r T ty2

For Wanteds, for /soundness/ of decomposition we need:
    ty1 ~r' ty2   ===>    T ty1 ~r T ty2
because if we do decompose we'll get evidence (co2 : ty1 ~r' ty2) and
from that we want to derive evidence (T co2 : T ty1 ~r T ty2).
For /completeness/ of decomposition we need the reverse implication too,
else we may decompose to a new proof obligation that is stronger than
the one we started with.  See Note [Decomposing newtype equalities].

---- Injectivity ----
When do these bi-implications hold? In one direction it is easy.
We /always/ have
    ty1 ~r'  ty2   ===>    T ty1 ~r T ty2
This is the CO_TYCONAPP rule of the paper (Fig 5); see also the
TyConAppCo case of GHC.Core.Lint.lintCoercion.

In the other direction, we have
    T ty1 ~r T ty2   ==>   ty1 ~r' ty2  if T is /injective at role r/
This is the very /definition/ of injectivity: injectivity means result
is the same => arguments are the same, modulo the role shift.
See comments on GHC.Core.TyCon.isInjectiveTyCon.  This is also
the CO_NTH rule in Fig 5 of the paper, except in the paper only
newtypes are non-injective at representation role, so the rule says "H
is not a newtype".

Injectivity is a bit subtle:
                 Nominal   Representational
   Datatype        YES        YES
   Newtype         YES        NO{1}
   Data family     YES        NO{2}

{1} Consider newtype N a = MkN (F a)   -- Arg has Nominal role
    Is it true that (N t1) ~R (N t2)   ==>   t1 ~N t2  ?
    No, absolutely not.  E.g.
       type instance F Int = Int; type instance F Bool = Char
       Then (N Int) ~R (N Bool), by unwrapping, but we don't want Int~Char!

    See Note [Decomposing newtype equalities]

{2} We must treat data families precisely like newtypes, because of the
    possibility of newtype instances. See also
    Note [Decomposing newtype equalities]. See #10534 and
    test case typecheck/should_fail/T10534.

---- Takeaway summary -----
For sound and complete decomposition, we simply need injectivity;
that is for isInjectiveTyCon to be true:

* At Nominal role, isInjectiveTyCon is True for all the TyCons we are
  considering in this Note: datatypes, newtypes, and data families.

* For Givens, injectivity is necessary for soundness; completeness has no
  side conditions.

* For Wanteds, soundness has no side conditions; but injectivity is needed
  for completeness. See Note [Decomposing newtype equalities]

This is implemented in `can_decompose` in `canTyConApp`; it looks at
injectivity, just as specified above.


Note [Decomposing type family applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Supose we have
   [G/W]  (F ty1) ~r  (F ty2)
This is handled by the TyFamLHS/TyFamLHS case of canEqCanLHS2.

We never decompose to
   [G/W]  ty1 ~r' ty2

Instead

* For Givens we do nothing. Injective type families have no corresponding
  evidence of their injectivity, so we cannot decompose an
  injective-type-family Given.

* For Wanteds, for the Nominal role only, we emit new Wanteds rather like
  functional dependencies, for each injective argument position.

  E.g type family F a b   -- injective in first arg, but not second
      [W] (F s1 t1) ~N (F s2 t2)
  Emit new Wanteds
      [W] s1 ~N s2
  But retain the existing, unsolved constraint.

Note [Decomposing newtype equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This Note also applies to data families, which we treat like
newtype in case of 'newtype instance'.

As Note [Decomposing TyConApp equalities] describes, if N is injective
at role r, we can do this decomposition?
   [G/W] (N ty1) ~r (N ty2)    to     [G/W]  ty1 ~r' ty2

For a Given with r=R, the answer is a solid NO: newtypes are not injective at
representational role, and we must not decompose, or we lose soundness.
Example is wrinkle {1} in Note [Decomposing TyConApp equalities].

For a Wanted with r=R, since newtypes are not injective at representational
role, decomposition is sound, but we may lose completeness.  Nevertheless,
if the newtype is abstract (so can't be unwrapped) we can only solve
the equality by (a) using a Given or (b) decomposition.  If (a) is impossible
(e.g. no Givens) then (b) is safe albeit potentially incomplete.

There are two ways in which decomposing (N ty1) ~r (N ty2) could be incomplete:

* Incompleteness example (EX1): unwrap first
      newtype Nt a = MkNt (Id a)
      type family Id a where Id a = a

      [W] Nt Int ~R Nt Age

  Because of its use of a type family, Nt's parameter will get inferred to
  have a nominal role. Thus, decomposing the wanted will yield [W] Int ~N Age,
  which is unsatisfiable. Unwrapping, though, leads to a solution.

  Conclusion: always unwrap newtypes before attempting to decompose
  them.  This is done in can_eq_nc'.  Of course, we can't unwrap if the data
  constructor isn't in scope.  See Note [Unwrap newtypes first].

* Incompleteness example (EX2): available Givens
      newtype Nt a = Mk Bool         -- NB: a is not used in the RHS,
      type role Nt representational  -- but the user gives it an R role anyway

      [G] Nt t1 ~R Nt t2
      [W] Nt alpha ~R Nt beta

  We *don't* want to decompose to [W] alpha ~R beta, because it's possible
  that alpha and beta aren't representationally equal.  And if we figure
  out (elsewhere) that alpha:=t1 and beta:=t2, we can solve the Wanted
  from the Given.  This is somewhat similar to the question of overlapping
  Givens for class constraints: see Note [Instance and Given overlap] in
  GHC.Tc.Solver.Interact.

  Conclusion: don't decompose [W] N s ~R N t, if there are any Given
  equalities that could later solve it.

  But what precisely does it mean to say "any Given equalities that could
  later solve it"?

  In #22924 we had
     [G] f a ~R# a     [W] Const (f a) a ~R# Const a a
  where Const is an abstract newtype.  If we decomposed the newtype, we
  could solve.  Not-decomposing on the grounds that (f a ~R# a) might turn
  into (Const (f a) a ~R# Const a a) seems a bit silly.

  In #22331 we had
     [G] N a ~R# N b   [W] N b ~R# N a
  (where N is abstract so we can't unwrap). Here we really /don't/ want to
  decompose, because the /only/ way to solve the Wanted is from that Given
  (with a Sym).

  In #22519 we had
     [G] a <= b     [W] IO Age ~R# IO Int

  (where IO is abstract so we can't unwrap, and newtype Age = Int; and (<=)
  is a type-level comparison on Nats).  Here we /must/ decompose, despite the
  existence of an Irred Given, or we will simply be stuck.  (Side note: We
  flirted with deep-rewriting of newtypes (see discussion on #22519 and
  !9623) but that turned out not to solve #22924, and also makes type
  inference loop more often on recursive newtypes.)

  The currently-implemented compromise is this:

    we decompose [W] N s ~R# N t unless there is a [G] N s' ~ N t'

  that is, a Given Irred equality with both sides headed with N.
  See the call to noGivenNewtypeReprEqs in canTyConApp.

  This is not perfect.  In principle a Given like [G] (a b) ~ (c d), or
  even just [G] c, could later turn into N s ~ N t.  But since the free
  vars of a Given are skolems, or at least untouchable unification
  variables, this is extremely unlikely to happen.

  Another worry: there could, just, be a CDictCan with some
  un-expanded equality superclasses; but only in some very obscure
  recursive-superclass situations.

   Yet another approach (!) is desribed in
   Note [Decomposing newtypes a bit more aggressively].

Remember: decomposing Wanteds is always /sound/. This Note is
only about /completeness/.

Note [Decomposing newtypes a bit more aggressively]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
IMPORTANT: the ideas in this Note are *not* implemented. Instead, the
current approach is detailed in Note [Decomposing newtype equalities]
and Note [Unwrap newtypes first].
For more details about the ideas in this Note see
  * GHC propoosal: https://github.com/ghc-proposals/ghc-proposals/pull/549
  * issue #22441
  * discussion on !9282.

Consider [G] c, [W] (IO Int) ~R (IO Age)
where IO is abstract, and
   newtype Age = MkAge Int   -- Not abstract
With the above rules, if there any Given Irreds,
the Wanted is insoluble because we can't decompose it.  But in fact,
if we look at the defn of IO, roughly,
    newtype IO a = State# -> (State#, a)
we can see that decomposing [W] (IO Int) ~R (IO Age) to
    [W] Int ~R Age
definitely does not lose completeness. Why not? Because the role of
IO's argment is representational.  Hence:

  DecomposeNewtypeIdea:
     decompose [W] (N s1 .. sn) ~R (N t1 .. tn)
     if the roles of all N's arguments are representational

If N's arguments really /are/ representational this will not lose
completeness.  Here "really are representational" means "if you expand
all newtypes in N's RHS, we'd infer a representational role for each
of N's type variables in that expansion".  See Note [Role inference]
in GHC.Tc.TyCl.Utils.

But the user might /override/ a phantom role with an explicit role
annotation, and then we could (obscurely) get incompleteness.
Consider

   module A( silly, T ) where
     newtype T a = MkT Int
     type role T representational  -- Override phantom role

     silly :: Coercion (T Int) (T Bool)
     silly = Coercion  -- Typechecks by unwrapping the newtype

     data Coercion a b where  -- Actually defined in Data.Type.Coercion
       Coercion :: Coercible a b => Coercion a b

   module B where
     import A
     f :: T Int -> T Bool
     f = case silly of Coercion -> coerce

Here the `coerce` gives [W] (T Int) ~R (T Bool) which, if we decompose,
we'll get stuck with (Int ~R Bool).  Instead we want to use the
[G] (T Int) ~R (T Bool), which will be in the Irreds.

Summary: we could adopt (DecomposeNewtypeIdea), at the cost of a very
obscure incompleteness (above).  But no one is reporting a problem from
the lack of decompostion, so we'll just leave it for now.  This long
Note is just to record the thinking for our future selves.

Note [Decomposing AppTy equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For AppTy all the same questions arise as in
Note [Decomposing TyConApp equalities]. We have

    s1 ~r s2,  t1 ~N t2   ==>   s1 t1 ~r s2 t2       (rule CO_APP)
    s1 t1 ~N s2 t2        ==>   s1 ~N s2,  t1 ~N t2  (CO_LEFT, CO_RIGHT)

In the first of these, why do we need Nominal equality in (t1 ~N t2)?
See {2} below.

For sound and complete solving, we need both directions to decompose. So:
* At nominal role, all is well: we have both directions.
* At representational role, decomposition of Givens is unsound (see {1} below),
  and decomposition of Wanteds is incomplete.

Here is an example of the incompleteness for Wanteds:

    [G] g1 :: a ~R b
    [W] w1 :: Maybe b ~R alpha a
    [W] w2 :: alpha ~N Maybe

Suppose we see w1 before w2. If we decompose, using AppCo to prove w1, we get

    w1 := AppCo w3 w4
    [W] w3 :: Maybe ~R alpha
    [W] w4 :: b ~N a

Note that w4 is *nominal*. A nominal role here is necessary because AppCo
requires a nominal role on its second argument. (See {2} for an example of
why.) Now we are stuck, because w4 is insoluble. On the other hand, if we
see w2 first, setting alpha := Maybe, all is well, as we can decompose
Maybe b ~R Maybe a into b ~R a.

Another example:
    newtype Phant x = MkPhant Int
    [W] w1 :: Phant Int ~R alpha Bool
    [W] w2 :: alpha ~ Phant

If we see w1 first, decomposing would be disastrous, as we would then try
to solve Int ~ Bool. Instead, spotting w2 allows us to simplify w1 to become
    [W] w1' :: Phant Int ~R Phant Bool

which can then (assuming MkPhant is in scope) be simplified to Int ~R Int,
and all will be well. See also Note [Unwrap newtypes first].

Bottom line:
* Always decompose AppTy at nominal role: can_eq_app
* Never decompose AppTy at representational role (neither Given nor Wanted):
  the lack of an equation in can_eq_nc'

Extra points
{1}  Decomposing a Given AppTy over a representational role is simply
     unsound. For example, if we have co1 :: Phant Int ~R a Bool (for
     the newtype Phant, above), then we surely don't want any relationship
     between Int and Bool, lest we also have co2 :: Phant ~ a around.

{2} The role on the AppCo coercion is a conservative choice, because we don't
    know the role signature of the function. For example, let's assume we could
    have a representational role on the second argument of AppCo. Then, consider

    data G a where    -- G will have a nominal role, as G is a GADT
      MkG :: G Int
    newtype Age = MkAge Int

    co1 :: G ~R a        -- by assumption
    co2 :: Age ~R Int    -- by newtype axiom
    co3 = AppCo co1 co2 :: G Age ~R a Int    -- by our broken AppCo

    and now co3 can be used to cast MkG to have type G Age, in violation of
    the way GADTs are supposed to work (which is to use nominal equality).
-}

canDecomposableTyConAppOK :: CtEvidence -> EqRel
                          -> TyCon -> [TcType] -> [TcType]
                          -> TcS (StopOrContinue Ct)
-- Precondition: tys1 and tys2 are the same finite length, hence "OK"
canDecomposableTyConAppOK ev eq_rel tc tys1 tys2
  = assert (tys1 `equalLength` tys2) $
    do { traceTcS "canDecomposableTyConAppOK"
                  (ppr ev $$ ppr eq_rel $$ ppr tc $$ ppr tys1 $$ ppr tys2)
       ; case ev of
           CtWanted { ctev_dest = dest, ctev_rewriters = rewriters }
                  -- new_locs and tc_roles are both infinite, so
                  -- we are guaranteed that cos has the same lengthm
                  -- as tys1 and tys2
                  -- See Note [Fast path when decomposing TyConApps]
                  -- Caution: unifyWanteds is order sensitive
                  -- See Note [Decomposing Dependent TyCons and Processing Wanted Equalities]
             -> do { cos <- unifyWanteds rewriters new_locs tc_roles tys1 tys2
                   ; setWantedEq dest (mkTyConAppCo role tc cos) }

           CtGiven { ctev_evar = evar }
             -> do { let ev_co = mkCoVarCo evar
                   ; given_evs <- newGivenEvVars loc $
                                  [ ( mkPrimEqPredRole r ty1 ty2
                                    , evCoercion $ mkSelCo (SelTyCon i r) ev_co )
                                  | (r, ty1, ty2, i) <- zip4 tc_roles tys1 tys2 [0..]
                                  , r /= Phantom
                                  , not (isCoercionTy ty1) && not (isCoercionTy ty2) ]
                   ; emitWorkNC given_evs }

    ; stopWith ev "Decomposed TyConApp" }

  where
    loc  = ctEvLoc ev
    role = eqRelRole eq_rel

    -- Infinite, to allow for over-saturated TyConApps
    tc_roles = tyConRoleListX role tc

      -- Add nuances to the location during decomposition:
      --  * if the argument is a kind argument, remember this, so that error
      --    messages say "kind", not "type". This is determined based on whether
      --    the corresponding tyConBinder is named (that is, dependent)
      --  * if the argument is invisible, note this as well, again by
      --    looking at the corresponding binder
      -- For oversaturated tycons, we need the (repeat loc) tail, which doesn't
      -- do either of these changes. (Forgetting to do so led to #16188)
      --
      -- NB: infinite in length
    new_locs = [ new_loc
               | bndr <- tyConBinders tc
               , let new_loc0 | isNamedTyConBinder bndr = toKindLoc loc
                              | otherwise               = loc
                     new_loc  | isInvisibleTyConBinder bndr
                              = updateCtLocOrigin new_loc0 toInvisibleOrigin
                              | otherwise
                              = new_loc0 ]
               ++ repeat loc

canDecomposableFunTy :: CtEvidence -> EqRel -> FunTyFlag
                     -> (Type,Type,Type)   -- (multiplicity,arg,res)
                     -> (Type,Type,Type)   -- (multiplicity,arg,res)
                     -> TcS (StopOrContinue Ct)
canDecomposableFunTy ev eq_rel af f1@(m1,a1,r1) f2@(m2,a2,r2)
  = do { traceTcS "canDecomposableFunTy"
                  (ppr ev $$ ppr eq_rel $$ ppr f1 $$ ppr f2)
       ; case ev of
           CtWanted { ctev_dest = dest, ctev_rewriters = rewriters }
             -> do { mult <- unifyWanted rewriters mult_loc (funRole role SelMult) m1 m2
                   ; arg  <- unifyWanted rewriters loc      (funRole role SelArg)  a1 a2
                   ; res  <- unifyWanted rewriters loc      (funRole role SelRes)  r1 r2
                   ; setWantedEq dest (mkNakedFunCo1 role af mult arg res) }

           CtGiven { ctev_evar = evar }
             -> do { let ev_co = mkCoVarCo evar
                   ; given_evs <- newGivenEvVars loc $
                                  [ ( mkPrimEqPredRole role' ty1 ty2
                                    , evCoercion $ mkSelCo (SelFun fs) ev_co )
                                  | (fs, ty1, ty2) <- [(SelMult, m1, m2)
                                                      ,(SelArg,  a1, a2)
                                                      ,(SelRes,  r1, r2)]
                                  , let role' = funRole role fs ]
                   ; emitWorkNC given_evs }

    ; stopWith ev "Decomposed TyConApp" }

  where
    loc      = ctEvLoc ev
    role     = eqRelRole eq_rel
    mult_loc = updateCtLocOrigin loc toInvisibleOrigin

-- | Call when canonicalizing an equality fails, but if the equality is
-- representational, there is some hope for the future.
-- Examples in Note [Use canEqFailure in canDecomposableTyConApp]
canEqFailure :: CtEvidence -> EqRel
             -> TcType -> TcType -> TcS (StopOrContinue Ct)
canEqFailure ev NomEq ty1 ty2
  = canEqHardFailure ev ty1 ty2
canEqFailure ev ReprEq ty1 ty2
  = do { (redn1, rewriters1) <- rewrite ev ty1
       ; (redn2, rewriters2) <- rewrite ev ty2
            -- We must rewrite the types before putting them in the
            -- inert set, so that we are sure to kick them out when
            -- new equalities become available
       ; traceTcS "canEqFailure with ReprEq" $
         vcat [ ppr ev, ppr redn1, ppr redn2 ]
       ; new_ev <- rewriteEqEvidence (rewriters1 S.<> rewriters2) ev NotSwapped redn1 redn2
       ; continueWith (mkIrredCt ReprEqReason new_ev) }

-- | Call when canonicalizing an equality fails with utterly no hope.
canEqHardFailure :: CtEvidence
                 -> TcType -> TcType -> TcS (StopOrContinue Ct)
-- See Note [Make sure that insolubles are fully rewritten]
canEqHardFailure ev ty1 ty2
  = do { traceTcS "canEqHardFailure" (ppr ty1 $$ ppr ty2)
       ; (redn1, rewriters1) <- rewriteForErrors ev ty1
       ; (redn2, rewriters2) <- rewriteForErrors ev ty2
       ; new_ev <- rewriteEqEvidence (rewriters1 S.<> rewriters2) ev NotSwapped redn1 redn2
       ; continueWith (mkIrredCt ShapeMismatchReason new_ev) }

{-
Note [Canonicalising type applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given (s1 t1) ~ ty2, how should we proceed?
The simple thing is to see if ty2 is of form (s2 t2), and
decompose.

However, over-eager decomposition gives bad error messages
for things like
   a b ~ Maybe c
   e f ~ p -> q
Suppose (in the first example) we already know a~Array.  Then if we
decompose the application eagerly, yielding
   a ~ Maybe
   b ~ c
we get an error        "Can't match Array ~ Maybe",
but we'd prefer to get "Can't match Array b ~ Maybe c".

So instead can_eq_wanted_app rewrites the LHS and RHS, in the hope of
replacing (a b) by (Array b), before using try_decompose_app to
decompose it.

Note [Make sure that insolubles are fully rewritten]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When an equality fails, we still want to rewrite the equality
all the way down, so that it accurately reflects
 (a) the mutable reference substitution in force at start of solving
 (b) any ty-binds in force at this point in solving
See Note [Rewrite insolubles] in GHC.Tc.Solver.InertSet.
And if we don't do this there is a bad danger that
GHC.Tc.Solver.applyTyVarDefaulting will find a variable
that has in fact been substituted.

Note [Do not decompose Given polytype equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider [G] (forall a. t1 ~ forall a. t2).  Can we decompose this?
No -- what would the evidence look like?  So instead we simply discard
this given evidence.

Note [No top-level newtypes on RHS of representational equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we're in this situation:

 work item:  [W] c1 : a ~R b
     inert:  [G] c2 : b ~R Id a

where
  newtype Id a = Id a

We want to make sure canEqCanLHS sees [W] a ~R a, after b is rewritten
and the Id newtype is unwrapped. This is assured by requiring only rewritten
types in canEqCanLHS *and* having the newtype-unwrapping check above
the tyvar check in can_eq_nc.

Note that this only applies to saturated applications of newtype TyCons, as
we can't rewrite an unsaturated application. See for example T22310, where
we ended up with:

  newtype Compose f g a = ...

  [W] t[tau] ~# Compose Foo Bar

Note [Put touchable variables on the left]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Ticket #10009, a very nasty example:

    f :: (UnF (F b) ~ b) => F b -> ()

    g :: forall a. (UnF (F a) ~ a) => a -> ()
    g _ = f (undefined :: F a)

For g we get [G]  g1 : UnF (F a) ~ a
             [W] w1 : UnF (F beta) ~ beta
             [W] w2 : F a ~ F beta

g1 is canonical (CEqCan). It is oriented as above because a is not touchable.
See canEqTyVarFunEq.

w1 is similarly canonical, though the occurs-check in canEqTyVarFunEq is key
here.

w2 is canonical. But which way should it be oriented? As written, we'll be
stuck. When w2 is added to the inert set, nothing gets kicked out: g1 is
a Given (and Wanteds don't rewrite Givens), and w2 doesn't mention the LHS
of w2. We'll thus lose.

But if w2 is swapped around, to

    [W] w3 : F beta ~ F a

then we'll kick w1 out of the inert
set (it mentions the LHS of w3). We then rewrite w1 to

    [W] w4 : UnF (F a) ~ beta

and then, using g1, to

    [W] w5 : a ~ beta

at which point we can unify and go on to glory. (This rewriting actually
happens all at once, in the call to rewrite during canonicalisation.)

But what about the new LHS makes it better? It mentions a variable (beta)
that can appear in a Wanted -- a touchable metavariable never appears
in a Given. On the other hand, the original LHS mentioned only variables
that appear in Givens. We thus choose to put variables that can appear
in Wanteds on the left.

Ticket #12526 is another good example of this in action.

-}

---------------------
canEqCanLHS :: CtEvidence            -- ev :: lhs ~ rhs
            -> EqRel -> SwapFlag
            -> CanEqLHS              -- lhs (or, if swapped, rhs)
            -> TcType                -- lhs: pretty lhs, already rewritten
            -> TcType -> TcType      -- rhs: already rewritten
            -> TcS (StopOrContinue Ct)
canEqCanLHS ev eq_rel swapped lhs1 ps_xi1 xi2 ps_xi2
  | k1 `tcEqType` k2
  = canEqCanLHSHomo ev eq_rel swapped lhs1 ps_xi1 xi2 ps_xi2

  | otherwise
  = canEqCanLHSHetero ev eq_rel swapped lhs1 k1 xi2 k2

  where
    k1 = canEqLHSKind lhs1
    k2 = typeKind xi2


{-
Note [Kind Equality Orientation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
While in theory [W] x ~ y and [W] y ~ x ought to give us the same behaviour, in practice it does not.
See Note [Fundeps with instances, and equality orientation] where this is discussed at length.
As a rule of thumb: we keep the newest unification variables on the left of the equality.
See also Note [Improvement orientation] in GHC.Tc.Solver.Interact.

In particular, `canEqCanLHSHetero` produces the following constraint equalities

[X] (xi1 :: ki1) ~ (xi2 :: ki2)
  -->  [X] kco :: ki1 ~ ki2
       [X] co : xi1 :: ki1 ~ (xi2 |> sym kco) :: ki1

Note that the types in the LHS of the new constraints are the ones that were on the LHS of
the original constraint.

--- Historical note ---
We prevously used to flip the kco to avoid using a sym in the cast

[X] (xi1 :: ki1) ~ (xi2 :: ki2)
  -->  [X] kco :: ki2 ~ ki1
       [X] co : xi1 :: ki1 ~ (xi2 |> kco) :: ki1

But this sent solver in an infinite loop (see #19415).
-- End of historical note --
-}

canEqCanLHSHetero :: CtEvidence         -- :: (xi1 :: ki1) ~ (xi2 :: ki2)
                  -> EqRel -> SwapFlag
                  -> CanEqLHS           -- xi1
                  -> TcKind             -- ki1
                  -> TcType             -- xi2
                  -> TcKind             -- ki2
                  -> TcS (StopOrContinue Ct)
canEqCanLHSHetero ev eq_rel swapped lhs1 ki1 xi2 ki2
  -- See Note [Equalities with incompatible kinds]
  -- See Note [Kind Equality Orientation]
  -- NB: preserve left-to-right orientation!!
  -- See Note [Fundeps with instances, and equality orientation]
  --     wrinkle (W2) in GHC.Tc.Solver.Interact
  = do { (kind_ev, kind_co) <- mk_kind_eq   -- :: ki1 ~N ki2

       ; let  -- kind_co :: (ki1 :: *) ~N (ki2 :: *)   (whether swapped or not)
             lhs_redn = mkReflRedn role xi1
             rhs_redn = mkGReflRightRedn role xi2 (mkSymCo kind_co)

             -- See Note [Equalities with incompatible kinds], Wrinkle (1)
             -- This will be ignored in rewriteEqEvidence if the work item is a Given
             rewriters = rewriterSetFromCo kind_co

       ; traceTcS "Hetero equality gives rise to kind equality"
           (ppr kind_co <+> dcolon <+> sep [ ppr ki1, text "~#", ppr ki2 ])
       ; type_ev <- rewriteEqEvidence rewriters ev swapped lhs_redn rhs_redn

       ; emitWorkNC [type_ev]  -- delay the type equality until after we've finished
                               -- the kind equality, which may unlock things
                               -- See Note [Equalities with incompatible kinds]

       ; solveNonCanonicalEquality kind_ev NomEq ki1 ki2 }
  where
    mk_kind_eq :: TcS (CtEvidence, CoercionN)
    mk_kind_eq = case ev of
      CtGiven { ctev_evar = evar }
        -> do { let kind_co = maybe_sym $ mkKindCo (mkCoVarCo evar) -- :: k1 ~ k2
              ; kind_ev <- newGivenEvVar kind_loc (kind_pty, evCoercion kind_co)
              ; return (kind_ev, ctEvCoercion kind_ev) }

      CtWanted { ctev_rewriters = rewriters }
        -> newWantedEq kind_loc rewriters Nominal ki1 ki2

    xi1      = canEqLHSType lhs1
    loc      = ctev_loc ev
    role     = eqRelRole eq_rel
    kind_loc = mkKindLoc xi1 xi2 loc
    kind_pty = mkHeteroPrimEqPred liftedTypeKind liftedTypeKind ki1 ki2

    maybe_sym = case swapped of
          IsSwapped  -> mkSymCo         -- if the input is swapped, then we
                                        -- will have k2 ~ k1, so flip it to k1 ~ k2
          NotSwapped -> id

canEqCanLHSHomo :: CtEvidence
                -> EqRel -> SwapFlag
                -> CanEqLHS           -- lhs (or, if swapped, rhs)
                -> TcType             -- pretty lhs
                -> TcType -> TcType   -- rhs, pretty rhs
                -> TcS (StopOrContinue Ct)
-- Guaranteed that typeKind lhs == typeKind rhs
canEqCanLHSHomo ev eq_rel swapped lhs1 ps_xi1 xi2 ps_xi2
  | (xi2', mco) <- split_cast_ty xi2
  , Just lhs2 <- canEqLHS_maybe xi2'
  = canEqCanLHS2 ev eq_rel swapped lhs1 ps_xi1 lhs2 (ps_xi2 `mkCastTyMCo` mkSymMCo mco) mco

  | otherwise
  = canEqCanLHSFinish ev eq_rel swapped lhs1 ps_xi2

  where
    split_cast_ty (CastTy ty co) = (ty, MCo co)
    split_cast_ty other          = (other, MRefl)

-- This function deals with the case that both LHS and RHS are potential
-- CanEqLHSs.
canEqCanLHS2 :: CtEvidence              -- lhs ~ (rhs |> mco)
                                        -- or, if swapped: (rhs |> mco) ~ lhs
             -> EqRel -> SwapFlag
             -> CanEqLHS                -- lhs (or, if swapped, rhs)
             -> TcType                  -- pretty lhs
             -> CanEqLHS                -- rhs
             -> TcType                  -- pretty rhs
             -> MCoercion               -- :: kind(rhs) ~N kind(lhs)
             -> TcS (StopOrContinue Ct)
canEqCanLHS2 ev eq_rel swapped lhs1 ps_xi1 lhs2 ps_xi2 mco
  | lhs1 `eqCanEqLHS` lhs2
    -- It must be the case that mco is reflexive
  = canEqReflexive ev eq_rel (canEqLHSType lhs1)

  | TyVarLHS tv1 <- lhs1
  , TyVarLHS tv2 <- lhs2
  = do { traceTcS "canEqLHS2 swapOver" (ppr tv1 $$ ppr tv2 $$ ppr swapped)
       ; if swapOverTyVars (isGiven ev) tv1 tv2
         then finish_with_swapping
         else finish_without_swapping }

  | TyVarLHS {} <- lhs1
  , TyFamLHS {} <- lhs2
  = if put_tyvar_on_lhs
    then finish_without_swapping
    else finish_with_swapping

  | TyFamLHS {} <- lhs1
  , TyVarLHS {} <- lhs2
  = if put_tyvar_on_lhs
    then finish_with_swapping
    else finish_without_swapping

  | TyFamLHS fun_tc1 fun_args1 <- lhs1
  , TyFamLHS fun_tc2 fun_args2 <- lhs2
  -- See Note [Decomposing type family applications]
  = do { traceTcS "canEqCanLHS2 two type families" (ppr lhs1 $$ ppr lhs2)

         -- emit wanted equalities for injective type families
       ; let inj_eqns :: [TypeEqn]  -- TypeEqn = Pair Type
             inj_eqns
               | ReprEq <- eq_rel   = []   -- injectivity applies only for nom. eqs.
               | fun_tc1 /= fun_tc2 = []   -- if the families don't match, stop.

               | Injective inj <- tyConInjectivityInfo fun_tc1
               = [ Pair arg1 arg2
                 | (arg1, arg2, True) <- zip3 fun_args1 fun_args2 inj ]

                 -- built-in synonym families don't have an entry point
                 -- for this use case. So, we just use sfInteractInert
                 -- and pass two equal RHSs. We *could* add another entry
                 -- point, but then there would be a burden to make
                 -- sure the new entry point and existing ones were
                 -- internally consistent. This is slightly distasteful,
                 -- but it works well in practice and localises the
                 -- problem.
               | Just ops <- isBuiltInSynFamTyCon_maybe fun_tc1
               = let ki1 = canEqLHSKind lhs1
                     ki2 | MRefl <- mco
                         = ki1   -- just a small optimisation
                         | otherwise
                         = canEqLHSKind lhs2

                     fake_rhs1 = anyTypeOfKind ki1
                     fake_rhs2 = anyTypeOfKind ki2
                 in
                 sfInteractInert ops fun_args1 fake_rhs1 fun_args2 fake_rhs2

               | otherwise  -- ordinary, non-injective type family
               = []

       ; case ev of
           CtWanted { ctev_rewriters = rewriters } ->
             mapM_ (\ (Pair t1 t2) -> unifyWanted rewriters (ctEvLoc ev) Nominal t1 t2) inj_eqns
           CtGiven {} -> return ()
             -- See Note [No Given/Given fundeps] in GHC.Tc.Solver.Interact

       ; tclvl <- getTcLevel
       ; let tvs1 = tyCoVarsOfTypes fun_args1
             tvs2 = tyCoVarsOfTypes fun_args2

             swap_for_rewriting = anyVarSet (isTouchableMetaTyVar tclvl) tvs2 &&
                          -- swap 'em: Note [Put touchable variables on the left]
                                  not (anyVarSet (isTouchableMetaTyVar tclvl) tvs1)
                          -- this check is just to avoid unfruitful swapping

             swap_for_occurs = False

{- ToDo: not sure about this
               -- If we have F a ~ F (F a), we want to swap.
             swap_for_occurs
               | cterHasNoProblem   $ checkTyFamEq fun_tc2 fun_args2
                                                   (mkTyConApp fun_tc1 fun_args1)
               , cterHasOccursCheck $ checkTyFamEq fun_tc1 fun_args1
                                                   (mkTyConApp fun_tc2 fun_args2)
               = True
               | otherwise
               = False
-}

       ; if swap_for_rewriting || swap_for_occurs
         then finish_with_swapping
         else finish_without_swapping }
  where
    sym_mco = mkSymMCo mco

    finish_without_swapping
      = canEqCanLHSFinish ev eq_rel swapped lhs1 (ps_xi2 `mkCastTyMCo` mco)
    finish_with_swapping
      = do { new_ev <- rewriteCastedEquality ev eq_rel swapped
                                (canEqLHSType lhs1) (canEqLHSType lhs2) mco
           ; canEqCanLHSFinish new_ev eq_rel IsSwapped lhs2 (ps_xi1 `mkCastTyMCo` sym_mco) }

    put_tyvar_on_lhs = isWanted ev && eq_rel == NomEq
    -- See Note [Orienting TyVarLHS/TyFamLHS]
    -- Same conditions as for canEqCanLHSFinish_try_unification
    -- which we are setting ourselves up for here

{- Note [Orienting TyVarLHS/TyFamLHS]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
What if one side is a TyVarLHS and the other is a TyFamLHS, (a ~ F tys)?
Which to put on the left?  Answer:
* Put the tyvar on the left, (a ~ F tys) as this may be our only shot to unify.
* But if we fail to unify and end up in cantMakeCanonical,
  then flip back to (F tys ~ a) because it's generally better
  to rewrite away function calls. This makes types smaller.

It's important to flip back. Consider
    [W] F alpha ~ alpha
    [W] F alpha ~ beta
    [W] G alpha beta ~ Int   ( where we have type instance G a a = a )
  If we end up with a stuck alpha ~ F alpha, we won't be able to solve this.
  Test case: indexed-types/should_compile/CEqCanOccursCheck
-}

-- The RHS here is either not CanEqLHS, or it's one that we
-- want to rewrite the LHS to (as per e.g. swapOverTyVars)
canEqCanLHSFinish, canEqCanLHSFinish_try_unification,
                   canEqCanLHSFinish_no_unification
    :: CtEvidence
    -> EqRel -> SwapFlag
    -> CanEqLHS             -- lhs (or, if swapped, rhs)
    -> TcType               -- rhs (or, if swapped, lhs)
    -> TcS (StopOrContinue Ct)
    -- RHS is fully rewritten, but with type synonyms
    --   preserved as much as possible
    -- Guaranteed preconditions that
    --    (TyEq:K)  handled in canEqCanLHSHomo
    --    (TyEq:N)  checked in can_eq_nc'
    --    (TyEq:TV) handled in canEqCanLHS2

---------------------------
canEqCanLHSFinish ev eq_rel swapped lhs rhs
  = do { traceTcS "canEqCanLHSFinish" $
         vcat [ text "ev:" <+> ppr ev
              , text "swapped:" <+> ppr swapped
              , text "lhs:" <+> ppr lhs
              , text "rhs:" <+> ppr rhs ]

         -- Assertion: (TyEq:K) is already satisfied
       ; massert (canEqLHSKind lhs `eqType` typeKind rhs)

         -- Assertion: (TyEq:N) is already satisfied (if applicable)
       ; assertPprM ty_eq_N_OK $
           vcat [ text "CanEqCanLHSFinish: (TyEq:N) not satisfied"
                , text "rhs:" <+> ppr rhs ]

       ; canEqCanLHSFinish_try_unification ev eq_rel swapped lhs rhs }

  where
    -- This is about (TyEq:N): check that we don't have a saturated application
    -- of a newtype TyCon at the top level of the RHS, if the constructor
    -- of the newtype is in scope.
    ty_eq_N_OK :: TcS Bool
    ty_eq_N_OK
      | ReprEq <- eq_rel
      , Just (tc, tc_args) <- splitTyConApp_maybe rhs
      , Just con <- newTyConDataCon_maybe tc
      -- #22310: only a problem if the newtype TyCon is saturated.
      , tc_args `lengthAtLeast` tyConArity tc
      -- #21010: only a problem if the newtype constructor is in scope.
      = do { rdr_env <- getGlobalRdrEnvTcS
           ; let con_in_scope = isJust $ lookupGRE_Name rdr_env (dataConName con)
           ; return $ not con_in_scope }
      | otherwise
      = return True

-----------------------
canEqCanLHSFinish_try_unification ev eq_rel swapped lhs rhs
  -- Try unification; for Wanted, Nominal equalities with a meta-tyvar on the LHS
  | isWanted ev      -- See Note [Do not unify Givens]
  , NomEq <- eq_rel  -- See Note [Do not unify representational equalities]
  , TyVarLHS tv <- lhs
  = do { given_eq_lvl <- getInnermostGivenEqLevel
       ; if not (touchabilityTest given_eq_lvl tv rhs)
         then canEqCanLHSFinish_no_unification ev eq_rel swapped lhs rhs
         else

    -- We have a touchable unification variable on the left
    do { check_result <- checkTouchableTyVarEq ev tv rhs
       ; case check_result of {
            PuFail reason -> cantMakeCanonical reason ev eq_rel swapped lhs rhs ;
            PuOK redn _   ->

    -- Success: we can solve by unification
    do { let tv_ty     = mkTyVarTy tv
             final_rhs = reductionReducedType redn
             tv_lvl    = tcTyVarLevel tv

       ; traceTcS "Sneaky unification:" $
         vcat [text "Unifies:" <+> ppr tv <+> text ":=" <+> ppr final_rhs,
               text "Coercion:" <+> pprEq tv_ty final_rhs,
               text "Left Kind is:" <+> ppr (typeKind tv_ty),
               text "Right Kind is:" <+> ppr (typeKind final_rhs) ]

       -- Update the unification variable itself
       ; unifyTyVar tv final_rhs

       -- Provide Refl evidence for the constraint
       -- Ignore 'swapped' because it's Refl!
       ; setEvBindIfWanted ev IsCoherent $
         evCoercion (mkNomReflCo final_rhs)

       -- Set the unification flag if we have done outer unifications
       -- that might affect an earlier implication constraint
       ; ambient_lvl <- getTcLevel
       ; when (ambient_lvl `strictlyDeeperThan` tv_lvl) $
         setUnificationFlag tv_lvl

       -- Kick out any constraints that can now be rewritten
       ; n_kicked <- kickOutAfterUnification tv

       ; return (Stop ev (text "Solved by unification" <+> pprKicked n_kicked)) }}}}

  -- Otherwise unification is off the table
  | otherwise
  = canEqCanLHSFinish_no_unification ev eq_rel swapped lhs rhs


---------------------------
-- Unification is off the table
canEqCanLHSFinish_no_unification ev eq_rel swapped lhs rhs
  = do { -- Do checkTypeEq to guarantee (TyEq:OC), (TyEq:F)
         -- Must do the occurs check even on tyvar/tyvar equalities,
         -- in case have  x ~ (y :: ..x...); this is #12593.
       ; check_result <- checkTypeEq ev eq_rel lhs rhs

       ; case check_result of {
            PuFail reason   -> cantMakeCanonical reason ev eq_rel swapped lhs rhs ;
            PuOK rhs_redn _ ->

    do { new_ev <- rewriteEqEvidence emptyRewriterSet ev swapped
                       (mkReflRedn Nominal (canEqLHSType lhs)) rhs_redn

       ; interactEq (EqCt { eq_ev  = new_ev, eq_eq_rel = eq_rel
                          , eq_lhs = lhs
                          , eq_rhs = reductionReducedType rhs_redn }) }}}

----------------------
cantMakeCanonical :: CheckTyEqResult -> CtEvidence -> EqRel -> SwapFlag
                  -> CanEqLHS -> TcType
                  -> TcS (StopOrContinue Ct)
cantMakeCanonical reason ev eq_rel swapped lhs rhs
  | TyVarLHS tv <- lhs
  , Just (tc,tys) <- splitTyConApp_maybe rhs
  , isFamilyTyCon tc
  , let lhs_ty = mkTyVarTy tv
  = -- Flip (a ~ F tys) to (F tys ~ a)
    do { new_ev <- rewriteEqEvidence emptyRewriterSet ev (flipSwap swapped)
                           (mkReflRedn role rhs) (mkReflRedn role lhs_ty)
       ; interactEq (EqCt { eq_ev  = new_ev, eq_eq_rel = eq_rel
                          , eq_lhs = TyFamLHS tc tys
                          , eq_rhs = lhs_ty }) }

  | otherwise
  = do { traceTcS "cantMakeCanonical" (ppr lhs $$ ppr rhs)
       ; new_ev <- rewriteEqEvidence emptyRewriterSet ev swapped
                           (mkReflRedn role (canEqLHSType lhs)) (mkReflRedn role rhs)
       ; solveIrredEquality (NonCanonicalReason reason) new_ev }
  where
    role = eqRelRole eq_rel

-----------------------
-- | Solve a reflexive equality constraint
canEqReflexive :: CtEvidence    -- ty ~ ty
               -> EqRel
               -> TcType        -- ty
               -> TcS (StopOrContinue Ct)   -- always Stop
canEqReflexive ev eq_rel ty
  = do { setEvBindIfWanted ev IsCoherent $
         evCoercion (mkReflCo (eqRelRole eq_rel) ty)
       ; stopWith ev "Solved by reflexivity" }

{- Note [Equalities with incompatible kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
What do we do when we have an equality

  (tv :: k1) ~ (rhs :: k2)

where k1 and k2 differ? Easy: we create a coercion that relates k1 and
k2 and use this to cast. To wit, from

  [X] (tv :: k1) ~ (rhs :: k2)

(where [X] is [G] or [W]), we go to

  [X] co :: k1 ~ k2
  [X] (tv :: k1) ~ ((rhs |> sym co) :: k1)

We carry on with the *kind equality*, not the type equality, because
solving the former may unlock the latter. This choice is made in
canEqCanLHSHetero. It is important: otherwise, T13135 loops.

Wrinkles:

 (1) When X is W, the new type-level wanted is effectively rewritten by the
     kind-level one. We thus include the kind-level wanted in the RewriterSet
     for the type-level one. See Note [Wanteds rewrite Wanteds] in GHC.Tc.Types.Constraint.
     This is done in canEqCanLHSHetero.

 (2) If we have [W] w :: alpha ~ (rhs |> sym co_hole), should we unify alpha? No.
     The problem is that the wanted w is effectively rewritten by another wanted,
     and unifying alpha effectively promotes this wanted to a given. Doing so
     means we lose track of the rewriter set associated with the wanted.

     On the other hand, w is perfectly suitable for rewriting, because of the
     way we carefully track rewriter sets.

     We thus allow w to be a CEqCan, but we prevent unification. See
     Note [Unification preconditions] in GHC.Tc.Utils.Unify.

     The only tricky part is that we must later indeed unify if/when the kind-level
     wanted gets solved. This is done in kickOutAfterFillingCoercionHole,
     which kicks out all equalities whose RHS mentions the filled-in coercion hole.
     Note that it looks for type family equalities, too, because of the use of
     unifyTest in canEqTyVarFunEq.

 (3) Suppose we have [W] (a :: k1) ~ (rhs :: k2). We duly follow the
     algorithm detailed here, producing [W] co :: k1 ~ k2, and adding
     [W] (a :: k1) ~ ((rhs |> sym co) :: k1) to the irreducibles. Some time
     later, we solve co, and fill in co's coercion hole. This kicks out
     the irreducible as described in (2).
     But now, during canonicalization, we see the cast
     and remove it, in canEqCast. By the time we get into canEqCanLHS, the equality
     is heterogeneous again, and the process repeats.

     To avoid this, we don't strip casts off a type if the other type
     in the equality is a CanEqLHS (the scenario above can happen with a
     type family, too. testcase: typecheck/should_compile/T13822).
     And this is an improvement regardless:
     because tyvars can, generally, unify with casted types, there's no
     reason to go through the work of stripping off the cast when the
     cast appears opposite a tyvar. This is implemented in the cast case
     of can_eq_nc'.

Historical note:

We used to do this via emitting a Derived kind equality and then parking
the heterogeneous equality as irreducible. But this new approach is much
more direct. And it doesn't produce duplicate Deriveds (as the old one did).

Note [Type synonyms and canonicalization]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We treat type synonym applications as xi types, that is, they do not
count as type function applications.  However, we do need to be a bit
careful with type synonyms: like type functions they may not be
generative or injective.  However, unlike type functions, they are
parametric, so there is no problem in expanding them whenever we see
them, since we do not need to know anything about their arguments in
order to expand them; this is what justifies not having to treat them
as specially as type function applications.  The thing that causes
some subtleties is that we prefer to leave type synonym applications
*unexpanded* whenever possible, in order to generate better error
messages.

If we encounter an equality constraint with type synonym applications
on both sides, or a type synonym application on one side and some sort
of type application on the other, we simply must expand out the type
synonyms in order to continue decomposing the equality constraint into
primitive equality constraints.  For example, suppose we have

  type F a = [Int]

and we encounter the equality

  F a ~ [b]

In order to continue we must expand F a into [Int], giving us the
equality

  [Int] ~ [b]

which we can then decompose into the more primitive equality
constraint

  Int ~ b.

However, if we encounter an equality constraint with a type synonym
application on one side and a variable on the other side, we should
NOT (necessarily) expand the type synonym, since for the purpose of
good error messages we want to leave type synonyms unexpanded as much
as possible.  Hence the ps_xi1, ps_xi2 argument passed to canEqCanLHS.

Note [Type equality cycles]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this situation (from indexed-types/should_compile/GivenLoop):

  instance C (Maybe b)
  *[G] a ~ Maybe (F a)
  [W] C a

or (typecheck/should_compile/T19682b):

  instance C (a -> b)
  *[W] alpha ~ (Arg alpha -> Res alpha)
  [W] C alpha

or (typecheck/should_compile/T21515):

  type family Code a
  *[G] Code a ~ '[ '[ Head (Head (Code a)) ] ]
  [W] Code a ~ '[ '[ alpha ] ]

In order to solve the final Wanted, we must use the starred constraint
for rewriting. But note that all starred constraints have occurs-check failures,
and so we can't straightforwardly add these to the inert set and
use them for rewriting. (NB: A rigid type constructor is at the
top of all RHSs, preventing reorienting in canEqTyVarFunEq in the tyvar
cases.)

The key idea is to replace the outermost type family applications in the RHS of the
starred constraints with a fresh variable, which we'll call a cycle-breaker
variable, or cbv. Then, relate the cbv back with the original type family application
via new equality constraints. Our situations thus become:

  instance C (Maybe b)
  [G] a ~ Maybe cbv
  [G] F a ~ cbv
  [W] C a

or

  instance C (a -> b)
  [W] alpha ~ (cbv1 -> cbv2)
  [W] Arg alpha ~ cbv1
  [W] Res alpha ~ cbv2
  [W] C alpha

or

  [G] Code a ~ '[ '[ cbv ] ]
  [G] Head (Head (Code a)) ~ cbv
  [W] Code a ~ '[ '[ alpha ] ]

This transformation (creating the new types and emitting new equality
constraints) is done in breakTyEqCycle_maybe.

The details depend on whether we're working with a Given or a Wanted.

Given
-----

We emit a new Given, [G] F a ~ cbv, equating the type family application to
our new cbv. Note its orientation: The type family ends up on the left; see
commentary on canEqTyVarFunEq, which decides how to orient such cases. No
special treatment for CycleBreakerTvs is necessary. This scenario is now
easily soluble, by using the first Given to rewrite the Wanted, which can now
be solved.

(The first Given actually also rewrites the second one, giving
[G] F (Maybe cbv) ~ cbv, but this causes no trouble.)

Of course, we don't want our fresh variables leaking into e.g. error messages.
So we fill in the metavariables with their original type family applications
after we're done running the solver (in nestImplicTcS and runTcSWithEvBinds).
This is done by restoreTyVarCycles, which uses the inert_cycle_breakers field in
InertSet, which contains the pairings invented in breakTyEqCycle_maybe.

That is:

We transform
  [G] g : lhs ~ ...(F lhs)...
to
  [G] (Refl lhs) : F lhs ~ cbv      -- CEqCan
  [G] g          : lhs ~ ...cbv...  -- CEqCan

Note that
* `cbv` is a fresh cycle breaker variable.
* `cbv` is a is a meta-tyvar, but it is completely untouchable.
* We track the cycle-breaker variables in inert_cycle_breakers in InertSet
* We eventually fill in the cycle-breakers, with `cbv := F lhs`.
  No one else fills in cycle-breakers!
* The evidence for the new `F lhs ~ cbv` constraint is Refl, because we know
  this fill-in is ultimately going to happen.
* In inert_cycle_breakers, we remember the (cbv, F lhs) pair; that is, we
  remember the /original/ type.  The [G] F lhs ~ cbv constraint may be rewritten
  by other givens (eg if we have another [G] lhs ~ (b,c)), but at the end we
  still fill in with cbv := F lhs
* This fill-in is done when solving is complete, by restoreTyVarCycles
  in nestImplicTcS and runTcSWithEvBinds.

Wanted
------
The fresh cycle-breaker variables here must actually be normal, touchable
metavariables. That is, they are TauTvs. Nothing at all unusual. Repeating
the example from above, we have

  *[W] alpha ~ (Arg alpha -> Res alpha)

and we turn this into

  *[W] alpha ~ (cbv1 -> cbv2)
  [W] Arg alpha ~ cbv1
  [W] Res alpha ~ cbv2

where cbv1 and cbv2 are fresh TauTvs. Why TauTvs? See [Why TauTvs] below.

Critically, we emit the two new constraints (the last two above)
directly instead of calling unifyWanted. (Otherwise, we'd end up unifying cbv1
and cbv2 immediately, achieving nothing.)
Next, we unify alpha := cbv1 -> cbv2, having eliminated the occurs check. This
unification -- which must be the next step after breaking the cycles --
happens in the course of normal behavior of top-level
interactions, later in the solver pipeline. We know this unification will
indeed happen because breakTyEqCycle_maybe, which decides whether to apply
this logic, checks to ensure unification will succeed in its final_check.
(In particular, the LHS must be a touchable tyvar, never a type family. We don't
yet have an example of where this logic is needed with a type family, and it's
unclear how to handle this case, so we're skipping for now.) Now, we're
here (including further context from our original example, from the top of the
Note):

  instance C (a -> b)
  [W] Arg (cbv1 -> cbv2) ~ cbv1
  [W] Res (cbv1 -> cbv2) ~ cbv2
  [W] C (cbv1 -> cbv2)

The first two W constraints reduce to reflexivity and are discarded,
and the last is easily soluble.

[Why TauTvs]:
Let's look at another example (typecheck/should_compile/T19682) where we need
to unify the cbvs:

  class    (AllEqF xs ys, SameShapeAs xs ys) => AllEq xs ys
  instance (AllEqF xs ys, SameShapeAs xs ys) => AllEq xs ys

  type family SameShapeAs xs ys :: Constraint where
    SameShapeAs '[] ys      = (ys ~ '[])
    SameShapeAs (x : xs) ys = (ys ~ (Head ys : Tail ys))

  type family AllEqF xs ys :: Constraint where
    AllEqF '[]      '[]      = ()
    AllEqF (x : xs) (y : ys) = (x ~ y, AllEq xs ys)

  [W] alpha ~ (Head alpha : Tail alpha)
  [W] AllEqF '[Bool] alpha

Without the logic detailed in this Note, we're stuck here, as AllEqF cannot
reduce and alpha cannot unify. Let's instead apply our cycle-breaker approach,
just as described above. We thus invent cbv1 and cbv2 and unify
alpha := cbv1 -> cbv2, yielding (after zonking)

  [W] Head (cbv1 : cbv2) ~ cbv1
  [W] Tail (cbv1 : cbv2) ~ cbv2
  [W] AllEqF '[Bool] (cbv1 : cbv2)

The first two W constraints simplify to reflexivity and are discarded.
But the last reduces:

  [W] Bool ~ cbv1
  [W] AllEq '[] cbv2

The first of these is solved by unification: cbv1 := Bool. The second
is solved by the instance for AllEq to become

  [W] AllEqF '[] cbv2
  [W] SameShapeAs '[] cbv2

While the first of these is stuck, the second makes progress, to lead to

  [W] AllEqF '[] cbv2
  [W] cbv2 ~ '[]

This second constraint is solved by unification: cbv2 := '[]. We now
have

  [W] AllEqF '[] '[]

which reduces to

  [W] ()

which is trivially satisfiable. Hooray!

Note that we need to unify the cbvs here; if we did not, there would be
no way to solve those constraints. That's why the cycle-breakers are
ordinary TauTvs.

In all cases
------------

We detect this scenario by the following characteristics:
 - a constraint with a soluble occurs-check failure
   (as indicated by the cteSolubleOccurs bit set in a CheckTyEqResult
   from checkTypeEq)
 - and a nominal equality
 - and either
    - a Given flavour (but see also Detail (7) below)
    - a Wanted flavour, with a touchable metavariable on the left

We don't use this trick for representational equalities, as there is no
concrete use case where it is helpful (unlike for nominal equalities).
Furthermore, because function applications can be CanEqLHSs, but newtype
applications cannot, the disparities between the cases are enough that it
would be effortful to expand the idea to representational equalities. A quick
attempt, with

      data family N a b

      f :: (Coercible a (N a b), Coercible (N a b) b) => a -> b
      f = coerce

failed with "Could not match 'b' with 'b'." Further work is held off
until when we have a concrete incentive to explore this dark corner.

Details:

 (1) We don't look under foralls, at all, when substituting away type family
     applications, because doing so can never be fruitful. Recall that we
     are in a case like [G] lhs ~ forall b. ... lhs ....   Until we have a type
     family that can pull the body out from a forall (e.g. type instance F (forall b. ty) = ty),
     this will always be
     insoluble. Note also that the forall cannot be in an argument to a
     type family, or that outer type family application would already have
     been substituted away.

     However, we still must check to make sure that breakTyEqCycle_maybe actually
     succeeds in getting rid of all occurrences of the offending lhs. If
     one is hidden under a forall, this won't be true. A similar problem can
     happen if the variable appears only in a kind
     (e.g. k ~ ... (a :: k) ...). So we perform an additional check after
     performing the substitution. It is tiresome to re-run all of checkTypeEq
     here, but reimplementing just the occurs-check is even more tiresome.

     Skipping this check causes typecheck/should_fail/GivenForallLoop and
     polykinds/T18451 to loop.

 (2) Our goal here is to avoid loops in rewriting. We can thus skip looking
     in coercions, as we don't rewrite in coercions in the algorithm in
     GHC.Solver.Rewrite. (This is another reason
     we need to re-check that we've gotten rid of all occurrences of the
     offending variable.)

 (3) As we're substituting as described in this Note, we can build ill-kinded
     types. For example, if we have Proxy (F a) b, where (b :: F a), then
     replacing this with Proxy cbv b is ill-kinded. However, we will later
     set cbv := F a, and so the zonked type will be well-kinded again.
     The temporary ill-kinded type hurts no one, and avoiding this would
     be quite painfully difficult.

     Specifically, this detail does not contravene the Purely Kinded Type Invariant
     (Note [The Purely Kinded Type Invariant (PKTI)] in GHC.Tc.Gen.HsType).
     The PKTI says that we can call typeKind on any type, without failure.
     It would be violated if we, say, replaced a kind (a -> b) with a kind c,
     because an arrow kind might be consulted in piResultTys. Here, we are
     replacing one opaque type like (F a b c) with another, cbv (opaque in
     that we never assume anything about its structure, like that it has a
     result type or a RuntimeRep argument).

 (4) The evidence for the produced Givens is all just reflexive, because
     we will eventually set the cycle-breaker variable to be the type family,
     and then, after the zonk, all will be well. See also the notes at the
     end of the Given section of this Note.

 (5) The approach here is inefficient because it replaces every (outermost)
     type family application with a type variable, regardless of whether that
     particular appplication is implicated in the occurs check.  An alternative
     would be to replce only type-family applications that mention the offending LHS.
     For instance, we could choose to
     affect only type family applications that mention the offending LHS:
     e.g. in a ~ (F b, G a), we need to replace only G a, not F b. Furthermore,
     we could try to detect cases like a ~ (F a, F a) and use the same
     tyvar to replace F a. (Cf.
     Note [Flattening type-family applications when matching instances]
     in GHC.Core.Unify, which
     goes to this extra effort.) There may be other opportunities for
     improvement. However, this is really a very small corner case.
     The investment to craft a clever,
     performant solution seems unworthwhile.

 (6) We often get the predicate associated with a constraint from its
     evidence with ctPred. We thus must not only make sure the generated
     CEqCan's fields have the updated RHS type (that is, the one produced
     by replacing type family applications with fresh variables),
     but we must also update the evidence itself. This is done by the call to rewriteEqEvidence
     in canEqCanLHSFinish.

 (7) We don't wish to apply this magic on the equalities created
     by this very same process.
     Consider this, from typecheck/should_compile/ContextStack2:

       type instance TF (a, b) = (TF a, TF b)
       t :: (a ~ TF (a, Int)) => ...

       [G] a ~ TF (a, Int)

     The RHS reduces, so we get

       [G] a ~ (TF a, TF Int)

     We then break cycles, to get

       [G] g1 :: a ~ (cbv1, cbv2)
       [G] g2 :: TF a ~ cbv1
       [G] g3 :: TF Int ~ cbv2

     g1 gets added to the inert set, as written. But then g2 becomes
     the work item. g1 rewrites g2 to become

       [G] TF (cbv1, cbv2) ~ cbv1

     which then uses the type instance to become

       [G] (TF cbv1, TF cbv2) ~ cbv1

     which looks remarkably like the Given we started with. If left
     unchecked, this will end up breaking cycles again, looping ad
     infinitum (and resulting in a context-stack reduction error,
     not an outright loop). The solution is easy: don't break cycles
     on an equality generated by breaking cycles. Instead, we mark this
     final Given as a CIrredCan with a NonCanonicalReason with the soluble
     occurs-check bit set (only).

     We track these equalities by giving them a special CtOrigin,
     CycleBreakerOrigin. This works for both Givens and Wanteds, as
     we need the logic in the W case for e.g. typecheck/should_fail/T17139.
     Because this logic needs to work for Wanteds, too, we cannot
     simply look for a CycleBreakerTv on the left: Wanteds don't use them.

 (8) We really want to do this all only when there is a soluble occurs-check
     failure, not when other problems arise (such as an impredicative
     equality like alpha ~ forall a. a -> a). That is why breakTyEqCycle_maybe
     uses cterHasOnlyProblem when looking at the result of checkTypeEq, which
     checks for many of the invariants on a CEqCan.


**********************************************************************
*                                                                    *
                   Rewriting evidence
*                                                                    *
**********************************************************************
-}

rewriteCastedEquality :: CtEvidence     -- :: lhs ~ (rhs |> mco), or (rhs |> mco) ~ lhs
                      -> EqRel -> SwapFlag
                      -> TcType         -- lhs
                      -> TcType         -- rhs
                      -> MCoercion      -- mco
                      -> TcS CtEvidence -- :: (lhs |> sym mco) ~ rhs
                                        -- result is independent of SwapFlag
rewriteCastedEquality ev eq_rel swapped lhs rhs mco
  = rewriteEqEvidence emptyRewriterSet ev swapped lhs_redn rhs_redn
  where
    lhs_redn = mkGReflRightMRedn role lhs sym_mco
    rhs_redn = mkGReflLeftMRedn  role rhs mco

    sym_mco = mkSymMCo mco
    role    = eqRelRole eq_rel

rewriteEqEvidence :: RewriterSet        -- New rewriters
                                        -- See GHC.Tc.Types.Constraint
                                        -- Note [Wanteds rewrite Wanteds]
                  -> CtEvidence         -- Old evidence :: olhs ~ orhs (not swapped)
                                        --              or orhs ~ olhs (swapped)
                  -> SwapFlag
                  -> Reduction          -- lhs_co :: olhs ~ nlhs
                  -> Reduction          -- rhs_co :: orhs ~ nrhs
                  -> TcS CtEvidence     -- Of type nlhs ~ nrhs
-- With reductions (Reduction lhs_co nlhs) (Reduction rhs_co nrhs),
-- rewriteEqEvidence yields, for a given equality (Given g olhs orhs):
-- If not swapped
--      g1 : nlhs ~ nrhs = sym lhs_co ; g ; rhs_co
-- If swapped
--      g1 : nlhs ~ nrhs = sym lhs_co ; Sym g ; rhs_co
--
-- For a wanted equality (Wanted w), we do the dual thing:
-- New  w1 : nlhs ~ nrhs
-- If not swapped
--      w : olhs ~ orhs = lhs_co ; w1 ; sym rhs_co
-- If swapped
--      w : orhs ~ olhs = rhs_co ; sym w1 ; sym lhs_co
--
-- It's all a form of rewriteEvidence, specialised for equalities
rewriteEqEvidence new_rewriters old_ev swapped (Reduction lhs_co nlhs) (Reduction rhs_co nrhs)
  | NotSwapped <- swapped
  , isReflCo lhs_co      -- See Note [Rewriting with Refl]
  , isReflCo rhs_co
  = return (setCtEvPredType old_ev new_pred)

  | CtGiven { ctev_evar = old_evar } <- old_ev
  = do { let new_tm = evCoercion ( mkSymCo lhs_co
                                  `mkTransCo` maybeSymCo swapped (mkCoVarCo old_evar)
                                  `mkTransCo` rhs_co)
       ; newGivenEvVar loc (new_pred, new_tm) }

  | CtWanted { ctev_dest = dest
             , ctev_rewriters = rewriters } <- old_ev
  , let rewriters' = rewriters S.<> new_rewriters
  = do { (new_ev, hole_co) <- newWantedEq loc rewriters'
                                          (ctEvRole old_ev) nlhs nrhs
       ; let co = maybeSymCo swapped $
                  lhs_co
                  `mkTransCo` hole_co
                  `mkTransCo` mkSymCo rhs_co
       ; setWantedEq dest co
       ; traceTcS "rewriteEqEvidence" (vcat [ ppr old_ev
                                            , ppr nlhs
                                            , ppr nrhs
                                            , ppr co
                                            , ppr new_rewriters ])
       ; return new_ev }

#if __GLASGOW_HASKELL__ <= 810
  | otherwise
  = panic "rewriteEvidence"
#endif
  where
    new_pred = mkTcEqPredLikeEv old_ev nlhs nrhs
    loc      = ctEvLoc old_ev

{-
**********************************************************************
*                                                                    *
                   interactEq
*                                                                    *
**********************************************************************

Note [Combining equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
   Inert:     g1 :: a ~ t
   Work item: g2 :: a ~ t

Then we can simply solve g2 from g1, thus g2 := g1.  Easy!
But it's not so simple:

* If t is a type variable, the equalties might be oriented differently:
      e.g. (g1 :: a~b) and (g2 :: b~a)
  So we look both ways round.  Hence the SwapFlag result to
  inertsCanDischarge.

* We can only do g2 := g1 if g1 can discharge g2; that depends on
  (a) the role and (b) the flavour.  E.g. a representational equality
  cannot discharge a nominal one; a Wanted cannot discharge a Given.
  The predicate is eqCanRewriteFR.

* Visibility. Suppose  S :: forall k. k -> Type, and consider unifying
      S @Type (a::Type)  ~   S @(Type->Type) (b::Type->Type)
  From the first argument we get (Type ~ Type->Type); from the second
  argument we get (a ~ b) which in turn gives (Type ~ Type->Type).
  See typecheck/should_fail/T16204c.

  That first argument is invisible in the source program (aside from
  visible type application), so we'd much prefer to get the error from
  the second. We track visibility in the uo_visible field of a TypeEqOrigin.
  We use this to prioritise visible errors (see GHC.Tc.Errors.tryReporters,
  the partition on isVisibleOrigin).

  So when combining two otherwise-identical equalites, we want to
  keep the visible one, and discharge the invisible one.  Hence the
  call to strictly_more_visible.
-}

interactEq :: EqCt -> TcS (StopOrContinue Ct)
interactEq work_item@(EqCt { eq_lhs = lhs, eq_ev = ev, eq_eq_rel = eq_rel })

  = do { inerts <- getInertCans
       ; if | Just (ev_i, swapped) <- inertsCanDischarge inerts work_item
            -> do { setEvBindIfWanted ev IsCoherent $
                    evCoercion (maybeSymCo swapped $
                                downgradeRole (eqRelRole eq_rel)
                                              (ctEvRole ev_i)
                                              (ctEvCoercion ev_i))
                  ; stopWith ev "Solved from inert" }

            | otherwise
            -> case lhs of
                 TyVarLHS {}      -> finishEqCt work_item
                 TyFamLHS tc args -> do { improveLocalFunEqs inerts tc args work_item
                                        ; improveTopFunEqs tc args work_item
                                        ; finishEqCt work_item } }


inertsCanDischarge :: InertCans -> EqCt
                   -> Maybe ( CtEvidence  -- The evidence for the inert
                            , SwapFlag )  -- Whether we need mkSymCo
inertsCanDischarge inerts (EqCt { eq_lhs = lhs_w, eq_rhs = rhs_w
                                , eq_ev = ev_w, eq_eq_rel = eq_rel })
  | (ev_i : _) <- [ ev_i | EqCt { eq_ev = ev_i, eq_rhs = rhs_i
                                , eq_eq_rel = eq_rel }
                                  <- findEq inerts lhs_w
                         , rhs_i `tcEqType` rhs_w
                         , inert_beats_wanted ev_i eq_rel ]
  =  -- Inert:     a ~ ty
     -- Work item: a ~ ty
    Just (ev_i, NotSwapped)

  | Just rhs_lhs <- canEqLHS_maybe rhs_w
  , (ev_i : _) <- [ ev_i | EqCt { eq_ev = ev_i, eq_rhs = rhs_i
                                , eq_eq_rel = eq_rel }
                             <- findEq inerts rhs_lhs
                         , rhs_i `tcEqType` canEqLHSType lhs_w
                         , inert_beats_wanted ev_i eq_rel ]
  =  -- Inert:     a ~ b
     -- Work item: b ~ a
     Just (ev_i, IsSwapped)

  where
    loc_w  = ctEvLoc ev_w
    flav_w = ctEvFlavour ev_w
    fr_w   = (flav_w, eq_rel)

    inert_beats_wanted ev_i eq_rel
      = -- eqCanRewriteFR:        see second bullet of Note [Combining equalities]
        -- strictly_more_visible: see last bullet of Note [Combining equalities]
        fr_i `eqCanRewriteFR` fr_w
        && not ((loc_w `strictly_more_visible` ctEvLoc ev_i)
                 && (fr_w `eqCanRewriteFR` fr_i))
      where
        fr_i = (ctEvFlavour ev_i, eq_rel)

    -- See Note [Combining equalities], final bullet
    strictly_more_visible loc1 loc2
       = not (isVisibleOrigin (ctLocOrigin loc2)) &&
         isVisibleOrigin (ctLocOrigin loc1)

inertsCanDischarge _ _ = Nothing



{- Note [Do not unify Givens]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this GADT match
   data T a where
      T1 :: T Int
      ...

   f x = case x of
           T1 -> True
           ...

So we get f :: T alpha[1] -> beta[1]
          x :: T alpha[1]
and from the T1 branch we get the implication
   forall[2] (alpha[1] ~ Int) => beta[1] ~ Bool

Now, clearly we don't want to unify alpha:=Int!  Yet at the moment we
process [G] alpha[1] ~ Int, we don't have any given-equalities in the
inert set, and hence there are no given equalities to make alpha untouchable.

NB: if it were alpha[2] ~ Int, this argument wouldn't hold.  But that
never happens: invariant (GivenInv) in Note [TcLevel invariants]
in GHC.Tc.Utils.TcType.

Simple solution: never unify in Givens!

Note [Do not unify representational equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider   [W] alpha ~R# b
where alpha is touchable. Should we unify alpha := b?

Certainly not!  Unifying forces alpha and be to be the same; but they
only need to be representationally equal types.

For example, we might have another constraint [W] alpha ~# N b
where
  newtype N b = MkN b
and we want to get alpha := N b.

See also #15144, which was caused by unifying a representational
equality.

Note [Solve by unification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we solve
   alpha[n] ~ ty
by unification, there are two cases to consider

* TouchableSameLevel: if the ambient level is 'n', then
  we can simply update alpha := ty, and do nothing else

* TouchableOuterLevel free_metas n: if the ambient level is greater than
  'n' (the level of alpha), in addition to setting alpha := ty we must
  do two other things:

  1. Promote all the free meta-vars of 'ty' to level n.  After all,
     alpha[n] is at level n, and so if we set, say,
          alpha[n] := Maybe beta[m],
     we must ensure that when unifying beta we do skolem-escape checks
     etc relevant to level n.  Simple way to do that: promote beta to
     level n.

  2. Set the Unification Level Flag to record that a level-n unification has
     taken place. See Note [The Unification Level Flag] in GHC.Tc.Solver.Monad

NB: TouchableSameLevel is just an optimisation for TouchableOuterLevel. Promotion
would be a no-op, and setting the unification flag unnecessarily would just
make the solver iterate more often.  (We don't need to iterate when unifying
at the ambient level because of the kick-out mechanism.)
-}

{-********************************************************************
*                                                                    *
          Final wrap-up for equalities
*                                                                    *
********************************************************************-}

{- Note [Looking up primitive equalities in quantified constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For equalities (a ~# b) look up (a ~ b), and then do a superclass
selection. This avoids having to support quantified constraints whose
kind is not Constraint, such as (forall a. F a ~# b)

See
 * Note [Evidence for quantified constraints] in GHC.Core.Predicate
 * Note [Equality superclasses in quantified constraints]
   in GHC.Tc.Solver.Canonical
-}

--------------------
solveIrredEquality :: CtIrredReason -> CtEvidence -> TcS (StopOrContinue Ct)
solveIrredEquality reason ev
  | EqPred eq_rel t1 t2 <- classifyPredType (ctEvPred ev)
  = final_qci_check (mkIrredCt reason ev) eq_rel t1 t2
    -- If the final_qci_check fails, we'll do continueWith on an IrredCt
    -- That in turn will go down the Irred pipeline, so which deals with
    -- the case where we have [G] Coercible (m a) (m b), and [W] m a ~R# m b
    -- When we de-pipeline Irreds we may have to adjust here

  | otherwise  -- All the calls come from in this module, where we deal
               -- only with equalities, so ctEvPred ev) must be an equality.
               -- Indeed, we could pass eq_rel, t1, t2 as arguments, to avoid
               -- this can't happen case, but it's not a hot path, and this is
               -- simple and robust
  = pprPanic "solveIrredEquality" (ppr ev)

--------------------
finishEqCt :: EqCt -> TcS (StopOrContinue Ct)
finishEqCt work_item@(EqCt { eq_lhs = lhs, eq_rhs = rhs, eq_eq_rel = eq_rel })
  = final_qci_check (CEqCan work_item) eq_rel (canEqLHSType lhs) rhs

--------------------
final_qci_check :: Ct -> EqRel -> TcType -> TcType -> TcS (StopOrContinue Ct)
-- The "final QCI check" checks to see if we have
--    [W] t1 ~# t2
-- and a Given quantified contraint like (forall a b. blah => a :~: b)
-- Why?  See Note [Looking up primitive equalities in quantified constraints]
final_qci_check work_ct eq_rel lhs rhs
  | isWanted ev
  , Just (cls, tys) <- boxEqPred eq_rel lhs rhs
  = do { res <- matchLocalInst (mkClassPred cls tys) loc
       ; case res of
           OneInst { cir_mk_ev = mk_ev }
             -> chooseInstance work_ct
                    (res { cir_mk_ev = mk_eq_ev cls tys mk_ev })
           _ -> continueWith work_ct }

  | otherwise
  = continueWith work_ct
  where
    ev  = ctEvidence work_ct
    loc = ctEvLoc ev

    mk_eq_ev cls tys mk_ev evs
      | sc_id : rest <- classSCSelIds cls  -- Just one superclass for this
      = assert (null rest) $ case (mk_ev evs) of
          EvExpr e -> EvExpr (Var sc_id `mkTyApps` tys `App` e)
          ev       -> pprPanic "mk_eq_ev" (ppr ev)
      | otherwise = pprPanic "finishEqCt" (ppr work_ct)

{-
**********************************************************************
*                                                                    *
    Functional dependencies for type families
*                                                                    *
**********************************************************************

Note [Reverse order of fundep equations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this scenario (from dependent/should_fail/T13135_simple):

  type Sig :: Type -> Type
  data Sig a = SigFun a (Sig a)

  type SmartFun :: forall (t :: Type). Sig t -> Type
  type family SmartFun sig = r | r -> sig where
    SmartFun @Type (SigFun @Type a sig) = a -> SmartFun @Type sig

  [W] SmartFun @kappa sigma ~ (Int -> Bool)

The injectivity of SmartFun allows us to produce two new equalities:

  [W] w1 :: Type ~ kappa
  [W] w2 :: SigFun @Type Int beta ~ sigma

for some fresh (beta :: SigType). The second Wanted here is actually
heterogeneous: the LHS has type Sig Type while the RHS has type Sig kappa.
Of course, if we solve the first wanted first, the second becomes homogeneous.

When looking for injectivity-inspired equalities, we work left-to-right,
producing the two equalities in the order written above. However, these
equalities are then passed into unifyWanted, which will fail, adding these
to the work list. However, crucially, the work list operates like a *stack*.
So, because we add w1 and then w2, we process w2 first. This is silly: solving
w1 would unlock w2. So we make sure to add equalities to the work
list in left-to-right order, which requires a few key calls to 'reverse'.

This treatment is also used for class-based functional dependencies, although
we do not have a program yet known to exhibit a loop there. It just seems
like the right thing to do.

When this was originally conceived, it was necessary to avoid a loop in T13135.
That loop is now avoided by continuing with the kind equality (not the type
equality) in canEqCanLHSHetero (see Note [Equalities with incompatible kinds]
in GHC.Tc.Solver.Canonical). However, the idea of working left-to-right still
seems worthwhile, and so the calls to 'reverse' remain.

Note [Improvement orientation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also Note [Fundeps with instances, and equality orientation], which describes
the Exact Same Problem, with the same solution, but for functional dependencies.

A very delicate point is the orientation of equalities
arising from injectivity improvement (#12522).  Suppose we have
  type family F x = t | t -> x
  type instance F (a, Int) = (Int, G a)
where G is injective; and wanted constraints

  [W] TF (alpha, beta) ~ fuv
  [W] fuv ~ (Int, <some type>)

The injectivity will give rise to constraints

  [W] gamma1 ~ alpha
  [W] Int ~ beta

The fresh unification variable gamma1 comes from the fact that we
can only do "partial improvement" here; see Section 5.2 of
"Injective type families for Haskell" (HS'15).

Now, it's very important to orient the equations this way round,
so that the fresh unification variable will be eliminated in
favour of alpha.  If we instead had
   [W] alpha ~ gamma1
then we would unify alpha := gamma1; and kick out the wanted
constraint.  But when we grough it back in, it'd look like
   [W] TF (gamma1, beta) ~ fuv
and exactly the same thing would happen again!  Infinite loop.

This all seems fragile, and it might seem more robust to avoid
introducing gamma1 in the first place, in the case where the
actual argument (alpha, beta) partly matches the improvement
template.  But that's a bit tricky, esp when we remember that the
kinds much match too; so it's easier to let the normal machinery
handle it.  Instead we are careful to orient the new
equality with the template on the left.  Delicate, but it works.

-}

--------------------
improveTopFunEqs :: TyCon -> [TcType] -> EqCt -> TcS ()
-- See Note [FunDep and implicit parameter reactions]
improveTopFunEqs fam_tc args (EqCt { eq_ev = ev, eq_rhs = rhs })

  | isGiven ev = return ()  -- See Note [No Given/Given fundeps]

  | otherwise
  = do { fam_envs <- getFamInstEnvs
       ; eqns <- improve_top_fun_eqs fam_envs fam_tc args rhs
       ; traceTcS "improveTopFunEqs" (vcat [ ppr fam_tc <+> ppr args <+> ppr rhs
                                          , ppr eqns ])
       ; mapM_ (\(Pair ty1 ty2) -> unifyWanted rewriters loc Nominal ty1 ty2)
               (reverse eqns) }
         -- Missing that `reverse` causes T13135 and T13135_simple to loop.
         -- See Note [Reverse order of fundep equations]

  where
    loc = bumpCtLocDepth (ctEvLoc ev)
        -- ToDo: this location is wrong; it should be FunDepOrigin2
        -- See #14778
    rewriters = ctEvRewriters ev

improve_top_fun_eqs :: FamInstEnvs
                    -> TyCon -> [TcType] -> TcType
                    -> TcS [TypeEqn]
improve_top_fun_eqs fam_envs fam_tc args rhs_ty
  | Just ops <- isBuiltInSynFamTyCon_maybe fam_tc
  = return (sfInteractTop ops args rhs_ty)

  -- see Note [Type inference for type families with injectivity]
  | isOpenTypeFamilyTyCon fam_tc
  , Injective injective_args <- tyConInjectivityInfo fam_tc
  , let fam_insts = lookupFamInstEnvByTyCon fam_envs fam_tc
  = -- it is possible to have several compatible equations in an open type
    -- family but we only want to derive equalities from one such equation.
    do { let improvs = buildImprovementData fam_insts
                           fi_tvs fi_tys fi_rhs (const Nothing)

       ; traceTcS "improve_top_fun_eqs2" (ppr improvs)
       ; concatMapM (injImproveEqns injective_args) $
         take 1 improvs }

  | Just ax <- isClosedSynFamilyTyConWithAxiom_maybe fam_tc
  , Injective injective_args <- tyConInjectivityInfo fam_tc
  = concatMapM (injImproveEqns injective_args) $
    buildImprovementData (fromBranches (co_ax_branches ax))
                         cab_tvs cab_lhs cab_rhs Just

  | otherwise
  = return []

  where
      in_scope = mkInScopeSet (tyCoVarsOfType rhs_ty)

      buildImprovementData
          :: [a]                     -- axioms for a TF (FamInst or CoAxBranch)
          -> (a -> [TyVar])          -- get bound tyvars of an axiom
          -> (a -> [Type])           -- get LHS of an axiom
          -> (a -> Type)             -- get RHS of an axiom
          -> (a -> Maybe CoAxBranch) -- Just => apartness check required
          -> [( [Type], Subst, [TyVar], Maybe CoAxBranch )]
             -- Result:
             -- ( [arguments of a matching axiom]
             -- , RHS-unifying substitution
             -- , axiom variables without substitution
             -- , Maybe matching axiom [Nothing - open TF, Just - closed TF ] )
      buildImprovementData axioms axiomTVs axiomLHS axiomRHS wrap =
          [ (ax_args, subst, unsubstTvs, wrap axiom)
          | axiom <- axioms
          , let ax_args = axiomLHS axiom
                ax_rhs  = axiomRHS axiom
                ax_tvs  = axiomTVs axiom
                in_scope1 = in_scope `extendInScopeSetList` ax_tvs
          , Just subst <- [tcUnifyTyWithTFs False in_scope1 ax_rhs rhs_ty]
          , let notInSubst tv = not (tv `elemVarEnv` getTvSubstEnv subst)
                unsubstTvs    = filter (notInSubst <&&> isTyVar) ax_tvs ]
                   -- The order of unsubstTvs is important; it must be
                   -- in telescope order e.g. (k:*) (a:k)

      injImproveEqns :: [Bool]
                     -> ([Type], Subst, [TyCoVar], Maybe CoAxBranch)
                     -> TcS [TypeEqn]
      injImproveEqns inj_args (ax_args, subst, unsubstTvs, cabr)
        = do { subst <- instFlexiX subst unsubstTvs
                  -- If the current substitution bind [k -> *], and
                  -- one of the un-substituted tyvars is (a::k), we'd better
                  -- be sure to apply the current substitution to a's kind.
                  -- Hence instFlexiX.   #13135 was an example.

             ; return [ Pair (substTy subst ax_arg) arg
                        -- NB: the ax_arg part is on the left
                        -- see Note [Improvement orientation]
                      | case cabr of
                          Just cabr' -> apartnessCheck (substTys subst ax_args) cabr'
                          _          -> True
                      , (ax_arg, arg, True) <- zip3 ax_args args inj_args ] }


improveLocalFunEqs :: InertCans -> TyCon -> [TcType] -> EqCt -> TcS ()
-- Generate improvement equalities, by comparing
-- the current work item with inert CFunEqs
-- E.g.   x + y ~ z,   x + y' ~ z   =>   [W] y ~ y'
--
-- See Note [FunDep and implicit parameter reactions]
improveLocalFunEqs inerts fam_tc args (EqCt { eq_ev = work_ev, eq_rhs = rhs })
  = unless (null improvement_eqns) $
    do { traceTcS "interactFunEq improvements: " $
                   vcat [ text "Eqns:" <+> ppr improvement_eqns
                        , text "Candidates:" <+> ppr funeqs_for_tc
                        , text "Inert eqs:" <+> ppr (inert_eqs inerts) ]
       ; emitFunDepWanteds (ctEvRewriters work_ev) improvement_eqns }
  where
    funeqs        = inert_funeqs inerts
    funeqs_for_tc :: [EqCt]
    funeqs_for_tc = [ funeq_ct | equal_ct_list <- findFunEqsByTyCon funeqs fam_tc
                               , funeq_ct <- equal_ct_list
                               , NomEq == eq_eq_rel funeq_ct ]
                                  -- representational equalities don't interact
                                  -- with type family dependencies
    work_loc      = ctEvLoc work_ev
    work_pred     = ctEvPred work_ev
    fam_inj_info  = tyConInjectivityInfo fam_tc

    --------------------
    improvement_eqns :: [FunDepEqn (CtLoc, RewriterSet)]
    improvement_eqns
      | Just ops <- isBuiltInSynFamTyCon_maybe fam_tc
      =    -- Try built-in families, notably for arithmethic
        concatMap (do_one_built_in ops rhs) funeqs_for_tc

      | Injective injective_args <- fam_inj_info
      =    -- Try improvement from type families with injectivity annotations
        concatMap (do_one_injective injective_args rhs) funeqs_for_tc

      | otherwise
      = []

    --------------------
    do_one_built_in ops rhs (EqCt { eq_lhs = TyFamLHS _ iargs, eq_rhs = irhs, eq_ev = inert_ev })
      | not (isGiven inert_ev && isGiven work_ev)  -- See Note [No Given/Given fundeps]
      = mk_fd_eqns inert_ev (sfInteractInert ops args rhs iargs irhs)

      | otherwise
      = []

    do_one_built_in _ _ _ = pprPanic "interactFunEq 1" (ppr fam_tc) -- TyVarLHS

    --------------------
    -- See Note [Type inference for type families with injectivity]
    do_one_injective inj_args rhs (EqCt { eq_lhs = TyFamLHS _ inert_args
                                        , eq_rhs = irhs, eq_ev = inert_ev })
      | not (isGiven inert_ev && isGiven work_ev) -- See Note [No Given/Given fundeps]
      , rhs `tcEqType` irhs
      = mk_fd_eqns inert_ev $ [ Pair arg iarg
                              | (arg, iarg, True) <- zip3 args inert_args inj_args ]
      | otherwise
      = []

    do_one_injective _ _ _ = pprPanic "interactFunEq 2" (ppr fam_tc)  -- TyVarLHS

    --------------------
    mk_fd_eqns :: CtEvidence -> [TypeEqn] -> [FunDepEqn (CtLoc, RewriterSet)]
    mk_fd_eqns inert_ev eqns
      | null eqns  = []
      | otherwise  = [ FDEqn { fd_qtvs = [], fd_eqs = eqns
                             , fd_pred1 = work_pred
                             , fd_pred2 = inert_pred
                             , fd_loc   = (loc, inert_rewriters) } ]
      where
        initial_loc  -- start with the location of the Wanted involved
          | isGiven work_ev = inert_loc
          | otherwise       = work_loc
        eqn_orig        = InjTFOrigin1 work_pred (ctLocOrigin work_loc) (ctLocSpan work_loc)
                                       inert_pred (ctLocOrigin inert_loc) (ctLocSpan inert_loc)
        eqn_loc         = setCtLocOrigin initial_loc eqn_orig
        inert_pred      = ctEvPred inert_ev
        inert_loc       = ctEvLoc inert_ev
        inert_rewriters = ctEvRewriters inert_ev
        loc = eqn_loc { ctl_depth = ctl_depth inert_loc `maxSubGoalDepth`
                                    ctl_depth work_loc }

{- Note [Type inference for type families with injectivity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have a type family with an injectivity annotation:
    type family F a b = r | r -> b

Then if we have an equality like F s1 t1 ~ F s2 t2,
we can use the injectivity to get a new Wanted constraint on
the injective argument
  [W] t1 ~ t2

That in turn can help GHC solve constraints that would otherwise require
guessing.  For example, consider the ambiguity check for
   f :: F Int b -> Int
We get the constraint
   [W] F Int b ~ F Int beta
where beta is a unification variable.  Injectivity lets us pick beta ~ b.

Injectivity information is also used at the call sites. For example:
   g = f True
gives rise to
   [W] F Int b ~ Bool
from which we can derive b.  This requires looking at the defining equations of
a type family, ie. finding equation with a matching RHS (Bool in this example)
and inferring values of type variables (b in this example) from the LHS patterns
of the matching equation.  For closed type families we have to perform
additional apartness check for the selected equation to check that the selected
is guaranteed to fire for given LHS arguments.

These new constraints are Wanted constraints, but we will not use the evidence.
We could go further and offer evidence from decomposing injective type-function
applications, but that would require new evidence forms, and an extension to
FC, so we don't do that right now (Dec 14).

We generate these Wanteds in three places, depending on how we notice the
injectivity.

1. When we have a [W] F tys1 ~ F tys2. This is handled in canEqCanLHS2, and
described in Note [Decomposing type family applications] in GHC.Tc.Solver.Canonical.

2. When we have [W] F tys1 ~ T and [W] F tys2 ~ T. Note that neither of these
constraints rewrites the other, as they have different LHSs. This is done
in improveLocalFunEqs, called during the interactWithInertsStage.

3. When we have [W] F tys ~ T and an equation for F that looks like F tys' = T.
This is done in improve_top_fun_eqs, called from the top-level reactions stage.

See also Note [Injective type families] in GHC.Core.TyCon

Note [Cache-caused loops]
~~~~~~~~~~~~~~~~~~~~~~~~~
It is very dangerous to cache a rewritten wanted family equation as 'solved' in our
solved cache (which is the default behaviour or xCtEvidence), because the interaction
may not be contributing towards a solution. Here is an example:

Initial inert set:
  [W] g1 : F a ~ beta1
Work item:
  [W] g2 : F a ~ beta2
The work item will react with the inert yielding the _same_ inert set plus:
    (i)   Will set g2 := g1 `cast` g3
    (ii)  Will add to our solved cache that [S] g2 : F a ~ beta2
    (iii) Will emit [W] g3 : beta1 ~ beta2
Now, the g3 work item will be spontaneously solved to [G] g3 : beta1 ~ beta2
and then it will react the item in the inert ([W] g1 : F a ~ beta1). So it
will set
      g1 := g ; sym g3
and what is g? Well it would ideally be a new goal of type (F a ~ beta2) but
remember that we have this in our solved cache, and it is ... g2! In short we
created the evidence loop:

        g2 := g1 ; g3
        g3 := refl
        g1 := g2 ; sym g3

To avoid this situation we do not cache as solved any workitems (or inert)
which did not really made a 'step' towards proving some goal. Solved's are
just an optimization so we don't lose anything in terms of completeness of
solving.
-}
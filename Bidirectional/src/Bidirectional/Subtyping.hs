{-# LANGUAGE LambdaCase #-}

module Bidirectional.Subtyping (isSubtypeOf) where

import Bidirectional.Context qualified as Ctx
import Bidirectional.FreshVar (FreshTypeVar)
import Bidirectional.FreshVar qualified as FreshVar
import Bidirectional.Language (Tipe (..), Var (NamedVar))
import Bidirectional.ReportTypeErrors (ReportTypeErrors)
import Bidirectional.ReportTypeErrors qualified as ReportTypeErrors

--------------------------------------------------------------------------------
-- Subtyping

{- | "A isSubtypeOf B" means "A is more polymorphic than B".
 In other words: A has some universal binders (TForalls), and there is a way
 to bind the variables on those universals such that we get B.

 (<:Unit, <:->)
 This function traverses the two types to make sure that they match. Functions
 have to match Functions, Units have to match Units.

 (<:ForallR)
 If we find a Forall on B, it means B is polymorphic. Here, B has the form
 "Forall x C", so the question we want to answer is "is A more polymorphic than
 Forall x C?"
 What we do here is:
 - "Pretend" we instatiated _x_ to something. This is done by placing _x_ in the
   context as a Universal.
 - In this new context, we can talk about C (the thing that was inside B's
   Forall) as a standalone type. B is clearly more polymorphic than C, since it
   had one extra Forall (the one we just bound in the context).
 - Check whether A is a subtype of B in this new context. This seems weird,
   why does that make sense? Let's see:
   * If A is _more polymorphic than B_, and _B is more polymorphic than C_,
     than indeed A should be more polymorphic than C.
   * If A was _not more polymorphic than B_, that means we could only make A and
     B match by carefully instantiating some Foralls in B to match A (or maybe
     not even then, if there are not enough Foralls). But we just bound one
     of these Foralls to a Universal, which means it's not "up for grabs"
     anymore. So even if A "less polymorphic" than B, and C is "less
     polymorphic" than B, this will not make A and C match because C will have
     references to a Universal that does not exist in A.

 (<:ForallL)
 Whenever we find a Forall on A, we know that the type underneath it has a
 "hole". The "hole" is a _opportunity for instantiating_ the type that is
 underneath the Forall. This hole is represented by an existential variable in
 the context.
 Why is finding a Forall in A different from finding a Forall in B?
 Well, we want to know if "A is more polymorphic than B". A Forall in B
 makes it _more polymorphic_, thus it makes the problem _more difficult_.
 A Forall in B is an _additional requirement that A must also be polymorphic
 in a fitting way._
 On the other hand, a Forall in A is _an opportunity to sacrifice polymorphism
 in order to fit with B_.
 The goal, really, is to get A to be the same as B. Each Forall in A is
 a chance to make it more similar to B. The existential variables track these
 chances, and every time we solve an existential, we get less polymorphic, but
 closer to the goal.

 (InstantiateL, InstantiateR)
 As we keep walking down the tree, we may find places where an unsolved
 existential variable (a hole) on type A corresponds to something else on type
 B, such as a TUnit or a Function. When that happens, we attempt to instantiate
 it: meaning that we will try to find a value that fits in the hole in type A,
 to make it match whatever we found on the corresponding part of type B.

 The instantiation process works by attaching solutions to the existential
 variables in the context.
-}
isSubtypeOf :: (ReportTypeErrors m, FreshTypeVar m) => Tipe -> Tipe -> Ctx.Ctx -> m Ctx.Ctx
isSubtypeOf = \a b ctx ->
  go (Ctx.substCtxInType ctx a) (Ctx.substCtxInType ctx b) ctx
 where
  -- <:Unit
  go TUnit TUnit ctx = pure ctx
  -- <:Var, <:Exvar
  go (TVar v1) (TVar v2) ctx
    | v1 == v2 = do
        Ctx.varIsBoundToAType ctx v1
        pure ctx
  -- InstantiateL
  go (TVar varA) typeB ctx
    | Just (Ctx.IsExistential Nothing) <- Ctx.lookupBinding varA ctx =
        do
          varA `Ctx.doesNotOccurFreeIn` typeB
          instantiateToSubtypeOf ctx varA typeB
  -- InstantiateR
  go typeA (TVar varB) ctx
    | Just (Ctx.IsExistential Nothing) <- Ctx.lookupBinding varB ctx =
        do
          varB `Ctx.doesNotOccurFreeIn` typeA
          instantiateToSupertypeOf ctx typeA varB
  -- <:->
  go (TFunction argA retA) (TFunction argB retB) ctx = do
    ctx' <- go argB argA ctx
    let retA' = Ctx.substCtxInType ctx' retA
    let retB' = Ctx.substCtxInType ctx' retB
    go retA' retB' ctx'
  -- <:ForallR
  go typeA (TForall univName univType) ctx = do
    let univVar = NamedVar univName
    extendedCtx <- Ctx.bindUniversal univVar ctx
    dirtyCtx <- go typeA univType extendedCtx
    pure $ Ctx.dropEntriesUntilBinding_ univVar dirtyCtx
  -- <=ForallL
  go (TForall univName univType) typeB ctx = do
    let existentialVar = NamedVar univName
    existentialContext <-
      pure ctx
        >>= Ctx.markExistential existentialVar
        >>= Ctx.bindOpenExistential existentialVar
    dirtyCtx <- go univType typeB existentialContext
    pure $ Ctx.dropEntriesUntilMarkerOf existentialVar dirtyCtx
  -- otherwise, fail!
  go a b _ctx =
    ReportTypeErrors.typeError
      [ "Type error."
      , ReportTypeErrors.ind [show a]
      , "is not a subtype of"
      , ReportTypeErrors.ind [show b]
      ]

-- | Instantiate the var to a subtype of the type
instantiateToSubtypeOf :: (FreshTypeVar m, ReportTypeErrors m) => Ctx.Ctx -> Var -> Tipe -> m Ctx.Ctx
instantiateToSubtypeOf ctx alpha = \case
  (TVar beta) -> case Ctx.lookupBinding beta ctx of
    Nothing ->
      ReportTypeErrors.typeError ["Unbound variable (2)", ReportTypeErrors.ind1 beta]
    Just (Ctx.HasType termType) ->
      ReportTypeErrors.typeError
        [ "Cannot instantiate existential to term variable."
        , ReportTypeErrors.ind1 beta
        , "has type"
        , ReportTypeErrors.ind1 termType
        ]
    -- InstLReach
    Just (Ctx.IsExistential Nothing)
      | Ctx.definedBefore ctx alpha beta ->
          Ctx.solveExistential beta (Ctx.MonoVar alpha) ctx
    -- InstLSolve (via solved variable)
    Just (Ctx.IsExistential (Just solved)) ->
      -- Really this should not happen, bacause we use substCtxInType before
      -- each recursion
      Ctx.solveExistential alpha solved ctx
    -- InstLSolve (existential refers to outer universal or unsolved existential)
    Just _ ->
      Ctx.solveExistential alpha (Ctx.MonoVar beta) ctx
  -- InstLSolve (via unit-as-monotype)
  TUnit -> Ctx.solveExistential alpha Ctx.MonoUnit ctx
  -- InstLArr
  (TFunction argType retType) -> do
    argExists <- FreshVar.freshVar "InstLArr_arg"
    retExists <- FreshVar.freshVar "InstLArr_ret"
    articulatedCtx <-
      Ctx.articulateExistential
        alpha
        [retExists, argExists]
        (Ctx.MonoFunction (Ctx.MonoVar argExists) (Ctx.MonoVar retExists))
        ctx
    ctxAfterArgument <- instantiateToSupertypeOf articulatedCtx argType argExists
    instantiateToSubtypeOf ctxAfterArgument retExists (Ctx.substCtxInType ctxAfterArgument retType)
  -- InstLAIIR
  (TForall univName univType) -> do
    let univVar = NamedVar univName
    extendedCtx <- Ctx.bindUniversal univVar ctx
    dirtyCtx <- instantiateToSubtypeOf extendedCtx alpha univType
    pure $ Ctx.dropEntriesUntilBinding_ univVar dirtyCtx

-- | Instantiate the var to a supertype of the type
instantiateToSupertypeOf :: (ReportTypeErrors m, FreshTypeVar m) => Ctx.Ctx -> Tipe -> Var -> m Ctx.Ctx
instantiateToSupertypeOf ctx = flip go
 where
  go alpha = \case
    (TVar beta) -> case Ctx.lookupBinding beta ctx of
      Nothing ->
        ReportTypeErrors.typeError ["Unbound variable (1)", ReportTypeErrors.ind1 beta]
      Just (Ctx.HasType termType) ->
        ReportTypeErrors.typeError
          [ "Cannot instantiate existential to term variable."
          , ReportTypeErrors.ind1 beta
          , "has type"
          , ReportTypeErrors.ind1 termType
          ]
      -- InstRReach
      Just (Ctx.IsExistential Nothing)
        | Ctx.definedBefore ctx alpha beta ->
            Ctx.solveExistential beta (Ctx.MonoVar alpha) ctx
      -- InstRSolve (via solved variable)
      Just (Ctx.IsExistential (Just solved)) ->
        -- Really this should not happen, bacause we use substCtxInType before
        -- each recursion
        Ctx.solveExistential alpha solved ctx
      -- InstRSolve (existential refers to outer universal or unsolved existential)
      Just _ ->
        Ctx.solveExistential alpha (Ctx.MonoVar beta) ctx
    -- InstRSolve (via unit-as-monotype)
    TUnit -> Ctx.solveExistential alpha Ctx.MonoUnit ctx
    -- InstRArr
    TFunction argType retType -> do
      argExists <- FreshVar.freshVar "InstRArr_arg"
      retExists <- FreshVar.freshVar "InstRArr_ret"
      articulatedCtx <-
        Ctx.articulateExistential
          alpha
          [retExists, argExists]
          (Ctx.MonoFunction (Ctx.MonoVar argExists) (Ctx.MonoVar retExists))
          ctx
      ctxAfterArgument <- instantiateToSubtypeOf articulatedCtx argExists argType
      instantiateToSupertypeOf ctxAfterArgument (Ctx.substCtxInType ctxAfterArgument retType) retExists
    -- InstRAIIL
    (TForall univName univType) -> do
      let univVar = NamedVar univName
      -- If alpha should be a supertype of a Forall, it cannot be a monotype,
      -- so indeed we should not solve the existential here!
      -- A new existential variable is created so that typechecking can proceeed
      -- inside the Forall type, but whatever is learned about this variable
      -- never escapes.
      -- This excerpt from page 8 helps:
      -- "Here, we introduce a new variable α^ to go under the universal
      --  quantifier; then, instantiation applies InstRReach to set α^, not β^.
      --  Hence, β^ is, correctly, not constrained by this subtyping problem"
      extendedCtx <-
        pure ctx
          >>= Ctx.markExistential univVar
          >>= Ctx.bindOpenExistential univVar
      dirtyCtx <- instantiateToSupertypeOf extendedCtx univType alpha
      pure $ Ctx.dropEntriesUntilMarkerOf univVar dirtyCtx

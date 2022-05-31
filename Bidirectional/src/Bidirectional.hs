{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Bidirectional where

import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except qualified as Except
import Data.Functor ((<&>))
import Bidirectional.Language
import Bidirectional.Context
import Bidirectional.ReportTypeErrors
import Bidirectional.FreshVar qualified as FreshVar

-- Effects stack
type TC a = ExceptT String FreshVar.Fresh a

runTC :: TC a -> Either String a
runTC = FreshVar.runFresh . Except.runExceptT

--------------------------------------------------------------------------------
-- Examples

expectRight :: TC a -> a
expectRight = either error id . runTC

test_id :: Expr
test_id = ELam "x" (EVar (NamedVar "x"))

test_const :: Expr
test_const = ELam "x" (ELam "y" (EVar (NamedVar "x")))

test_constUnit :: Expr
test_constUnit = EApply test_const EUnit

{-

>>> expectRight . typeComplete $ EApply (ELam "f" (EApply (EVar (NamedVar "f")) EUnit)) test_id
TUnit

>>> expectRight . typeComplete $ test_id
TFunction (TVar (FreshVar "->I\8658_arg" 0)) (TVar (FreshVar "->I\8658_arg" 0))

>>> expectRight . typeComplete $ EAnno test_id (TForall "T" (TFunction (TVar (NamedVar "T")) (TVar (NamedVar "T"))))
TForall "T" (TFunction (TVar (NamedVar "T")) (TVar (NamedVar "T")))

>>> expectRight . typeComplete $ test_const
TFunction (TVar (FreshVar "->I\8658_arg" 0)) (TFunction (TVar (FreshVar "InstRArr_arg" 4)) (TVar (FreshVar "->I\8658_arg" 0)))

>>> expectRight . typeComplete $ ELam "x" EUnit
TFunction (TVar (FreshVar "->I\8658_arg" 0)) TUnit

>>> expectRight . typeComplete $ test_constUnit
TFunction (TVar (FreshVar "InstRArr_arg" 4)) TUnit

-}

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
isSubtypeOf :: Ctx -> Tipe -> Tipe -> TC Ctx
isSubtypeOf ctx = go
 where
  -- <:Unit
  go TUnit TUnit = pure ctx
  -- <:Var, <:Exvar
  go (TVar v1) (TVar v2)
    | v1 == v2 = do
        varIsBoundToAType ctx v1
        pure ctx
  -- InstantiateL
  go (TVar varA) typeB
    | Just (IsExistential Nothing) <- lookupBinding varA ctx =
        do
          varA `doesNotOccurFreeIn` typeB
          instantiateToSubtypeOf ctx varA typeB
  -- InstantiateR
  go typeA (TVar varB)
    | Just (IsExistential Nothing) <- lookupBinding varB ctx =
        do
          varB `doesNotOccurFreeIn` typeA
          instantiateToSupertypeOf ctx typeA varB
  -- <:->
  go (TFunction argA retA) (TFunction argB retB) = do
    ctx' <- isSubtypeOf ctx argB argA
    let retA' = substCtxInType ctx' retA
    let retB' = substCtxInType ctx' retB
    isSubtypeOf ctx' retA' retB'
  -- <:ForallR
  go typeA (TForall univName univType) = do
    let univVar = NamedVar univName
    extendedCtx <- bindUniversal univVar ctx
    dirtyCtx <- isSubtypeOf extendedCtx typeA univType
    pure $ dropEntriesUntilBinding_ univVar dirtyCtx
  -- <=ForallL
  go (TForall univName univType) typeB = do
    let existentialVar = NamedVar univName
    existentialContext <-
      pure ctx
        >>= markExistential existentialVar
        >>= bindOpenExistential existentialVar
    dirtyCtx <- isSubtypeOf existentialContext univType typeB
    pure $ dropEntriesUntilMarkerOf existentialVar dirtyCtx
  -- otherwise, fail!
  go a b =
    typeError
      [ "Type error."
      , ind [show a]
      , "is not a subtype of"
      , ind [show b]
      ]

-- | Instantiate the var to a subtype of the type
instantiateToSubtypeOf :: Ctx -> Var -> Tipe -> TC Ctx
instantiateToSubtypeOf ctx alpha = \case
  (TVar beta) -> case lookupBinding beta ctx of
    Nothing ->
      typeError ["Unbound variable (2)", ind1 beta]
    Just (HasType termType) ->
      typeError
        [ "Cannot instantiate existential to term variable."
        , ind1 beta
        , "has type"
        , ind1 termType
        ]
    -- InstLReach
    Just (IsExistential Nothing)
      | definedBefore ctx alpha beta ->
          solveExistential beta (MonoVar alpha) ctx
    -- InstLSolve (via solved variable)
    Just (IsExistential (Just solved)) ->
      -- Really this should not happen, bacause we use substCtxInType before
      -- each recursion
      solveExistential alpha solved ctx
    -- InstLSolve (existential refers to outer universal or unsolved existential)
    Just _ ->
      solveExistential alpha (MonoVar beta) ctx
  -- InstLSolve (via unit-as-monotype)
  TUnit -> solveExistential alpha MonoUnit ctx
  -- InstLArr
  (TFunction argType retType) -> do
    argExists <- FreshVar.freshVar "InstLArr_arg"
    retExists <- FreshVar.freshVar "InstLArr_ret"
    articulatedCtx <-
      articulateExistential
        alpha
        [retExists, argExists]
        (MonoFunction (MonoVar argExists) (MonoVar retExists))
        ctx
    ctxAfterArgument <- instantiateToSupertypeOf articulatedCtx argType argExists
    instantiateToSubtypeOf ctxAfterArgument retExists (substCtxInType ctxAfterArgument retType)
  -- InstLAIIR
  (TForall univName univType) -> do
    let univVar = NamedVar univName
    extendedCtx <- bindUniversal univVar ctx
    dirtyCtx <- instantiateToSubtypeOf extendedCtx alpha univType
    pure $ dropEntriesUntilBinding_ univVar dirtyCtx

-- | Instantiate the var to a supertype of the type
instantiateToSupertypeOf :: Ctx -> Tipe -> Var -> TC Ctx
instantiateToSupertypeOf ctx = flip go
 where
  go alpha = \case
    (TVar beta) -> case lookupBinding beta ctx of
      Nothing ->
        typeError ["Unbound variable (1)", ind1 beta]
      Just (HasType termType) ->
        typeError
          [ "Cannot instantiate existential to term variable."
          , ind1 beta
          , "has type"
          , ind1 termType
          ]
      -- InstRReach
      Just (IsExistential Nothing)
        | definedBefore ctx alpha beta ->
            solveExistential beta (MonoVar alpha) ctx
      -- InstRSolve (via solved variable)
      Just (IsExistential (Just solved)) ->
        -- Really this should not happen, bacause we use substCtxInType before
        -- each recursion
        solveExistential alpha solved ctx
      -- InstRSolve (existential refers to outer universal or unsolved existential)
      Just _ ->
        solveExistential alpha (MonoVar beta) ctx
    -- InstRSolve (via unit-as-monotype)
    TUnit -> solveExistential alpha MonoUnit ctx
    -- InstRArr
    TFunction argType retType -> do
      argExists <- FreshVar.freshVar "InstRArr_arg"
      retExists <- FreshVar.freshVar "InstRArr_ret"
      articulatedCtx <-
        articulateExistential
          alpha
          [retExists, argExists]
          (MonoFunction (MonoVar argExists) (MonoVar retExists))
          ctx
      ctxAfterArgument <- instantiateToSubtypeOf articulatedCtx argExists argType
      instantiateToSupertypeOf ctxAfterArgument (substCtxInType ctxAfterArgument retType) retExists
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
          >>= markExistential univVar
          >>= bindOpenExistential univVar
      dirtyCtx <- instantiateToSupertypeOf extendedCtx univType alpha
      pure $ dropEntriesUntilMarkerOf univVar dirtyCtx

--------------------------------------------------------------------------------
-- Type checking

typeComplete :: Expr -> TC Tipe
typeComplete expr = do
  (finalCtx, tipe) <- typeSynth emptyCtx expr
  pure $ substCtxInType finalCtx tipe

typeSynth :: Ctx -> Expr -> TC (Ctx, Tipe)
typeSynth ctx = goSynth
 where
  goSynth :: Expr -> TC (Ctx, Tipe)
  goSynth = \case
    -- Var
    EVar varName -> (ctx,) <$> varIsBoundToATerm ctx varName
    -- Anno
    EAnno expr tipe -> do
      isWellFormedType ctx tipe
      ctx' <- typeCheck ctx expr tipe
      pure (ctx', tipe)
    -- ->E
    expr@(EApply callee argument) -> do
      (ctxAfterCallee, calleeType) <- goSynth callee
      typeApply expr ctxAfterCallee (substCtxInType ctxAfterCallee calleeType) argument
    -- 1I⇒
    EUnit -> pure (ctx, TUnit)
    -- ->I⇒
    ELam argName body -> do
      let argVar = NamedVar argName
      alpha <- FreshVar.freshVar "->I⇒_arg"
      let argType = TVar alpha
      beta <- FreshVar.freshVar "->I⇒_ret"
      let retType = TVar beta
      extendedCtx <-
        pure ctx
          >>= bindOpenExistential alpha
          >>= bindOpenExistential beta
          >>= bindTermVar argVar argType
      dirtyCtx <- typeCheck extendedCtx body retType
      pure
        ( dropEntriesUntilBinding_ argVar dirtyCtx
        , TFunction argType retType
        )

typeCheck :: Ctx -> Expr -> Tipe -> TC Ctx
typeCheck ctx = goCheck
 where
  -- ForallI
  goCheck expr (TForall univName univType) = do
    let forallVar = NamedVar univName
    extendedCtx <- bindUniversal forallVar ctx
    dirtyCtx <- typeCheck extendedCtx expr univType
    pure (dropEntriesUntilBinding_ forallVar dirtyCtx)
  -- ->I
  goCheck (ELam argName body) (TFunction argType retType) = do
    let argVar = NamedVar argName
    extendedCtx <- bindTermVar argVar argType ctx
    dirtyCtx <- typeCheck extendedCtx body retType
    pure (dropEntriesUntilBinding_ argVar dirtyCtx)
  -- 1I
  goCheck EUnit TUnit = pure ctx
  -- Sub
  goCheck expr expectedType = do
    (ctxAfterSynth, foundType) <- typeSynth ctx expr
    isSubtypeOf
      ctxAfterSynth
      (substCtxInType ctxAfterSynth foundType)
      (substCtxInType ctxAfterSynth expectedType)
      `Except.catchE` \err ->
        typeError
          [ err
          , "when typechecking expression"
          , ind [show expr]
          ]

typeApply :: Expr -> Ctx -> Tipe -> Expr -> TC (Ctx, Tipe)
typeApply overallApplyExpr = goApply
 where
  -- ForallApp
  goApply ctx (TForall univName univType) argument = do
    extendedCtx <- bindOpenExistential (NamedVar univName) ctx
    goApply extendedCtx univType argument
  -- ->App
  goApply ctx (TFunction argType retType) argument = do
    typeCheck ctx argument argType <&> (,retType)
  -- ExistApp
  goApply ctx (TVar alpha) argument
    | Just (IsExistential Nothing) <- lookupBinding alpha ctx = do
        argVar <- FreshVar.freshVar "ExistApp_arg"
        retVar <- FreshVar.freshVar "ExistApp_ret"
        articulatedCtx <-
          articulateExistential
            alpha
            [retVar, argVar]
            (MonoFunction (MonoVar argVar) (MonoVar retVar))
            ctx
        typeCheck articulatedCtx argument (TVar argVar) <&> (,TVar retVar)
  goApply _ctx other _ =
    typeError $
      [ "Expected polymorphic or function type, but got"
      , ind [show other]
      , "in application expression"
      , ind [show overallApplyExpr]
      ]

substType :: Var -> Tipe -> Tipe -> Tipe
substType replacedVar replacement = go
 where
  go TUnit = TUnit
  go (TFunction argType retType) = TFunction (go argType) (go retType)
  go t@(TVar var)
    | var == replacedVar = replacement
    | otherwise = t
  go t@(TForall boundVarName boundType)
    -- shadowing
    | NamedVar boundVarName == replacedVar = t
    -- no shadowing
    | otherwise = TForall boundVarName (go boundType)

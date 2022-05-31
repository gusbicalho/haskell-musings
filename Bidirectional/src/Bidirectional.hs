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
  go (TForall boundName typeA) typeB = do
    existentialVar <- FreshVar.freshVar "<=ForallL"
    existentialContext <-
      pure ctx
        >>= markExistential existentialVar
        >>= bindOpenExistential existentialVar
    let boundA = substType (NamedVar boundName) (TVar existentialVar) typeA
    dirtyCtx <- isSubtypeOf existentialContext boundA typeB
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

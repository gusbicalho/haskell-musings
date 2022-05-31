{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module BidirectionalWithImplicitCtx where

import Bidirectional.Context (Ctx)
import Bidirectional.Context qualified as Ctx
import Bidirectional.ContextState qualified as CtxState
import Bidirectional.FreshVar qualified as FreshVar
import Bidirectional.Language
import Bidirectional.ReportTypeErrors
import Control.Monad.Trans.Except (Except)
import Control.Monad.Trans.Except qualified as Except
import Data.Functor ((<&>))

-- Effects stack
type TC = FreshVar.FreshT (Except String)

runTC :: TC a -> Either String a
runTC = Except.runExcept . FreshVar.runFreshT

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
--
-- Here I decided to manage Ctx explicitly in the top-level defs, because there
-- is a lot of checking the initial state in pattern matches, and also places
-- where we need to _speculatively_ extend the Ctx, but we don't want to
-- propagate changes (e.g. ForallWF).
-- I used in a few places `execCtx` to bridge the world with the implicit Ctx
-- management machinery defined above.

isSubtypeOf :: Ctx -> Tipe -> Tipe -> TC Ctx
isSubtypeOf ctx = go
 where
  -- <:Unit
  go TUnit TUnit = pure ctx
  -- <:Var, <:Exvar
  go (TVar v1) (TVar v2)
    | v1 == v2 = do
        Ctx.varIsBoundToAType ctx v1
        pure ctx
  -- InstantiateL
  go (TVar varA) typeB
    | Just (Ctx.IsExistential Nothing) <- Ctx.lookupBinding varA ctx =
        do
          varA `Ctx.doesNotOccurFreeIn` typeB
          instantiateToSubtypeOf varA typeB ctx
  -- InstantiateR
  go typeA (TVar varB)
    | Just (Ctx.IsExistential Nothing) <- Ctx.lookupBinding varB ctx =
        do
          varB `Ctx.doesNotOccurFreeIn` typeA
          instantiateToSupertypeOf typeA varB ctx
  -- <:->
  go (TFunction argA retA) (TFunction argB retB) = do
    ctx' <- isSubtypeOf ctx argB argA
    let retA' = Ctx.substCtxInType ctx' retA
    let retB' = Ctx.substCtxInType ctx' retB
    isSubtypeOf ctx' retA' retB'
  -- <:ForallR
  go typeA (TForall univName univType) = do
    let univVar = NamedVar univName
    extendedCtx <- CtxState.execCtx ctx $ CtxState.bindUniversal univVar
    dirtyCtx <- isSubtypeOf extendedCtx typeA univType
    pure $ Ctx.dropEntriesUntilBinding_ univVar dirtyCtx
  -- <=ForallL
  go (TForall boundName typeA) typeB = do
    existentialVar <- FreshVar.freshVar "<=ForallL"
    existentialContext <- CtxState.execCtx ctx do
      CtxState.markExistential existentialVar
      CtxState.bindOpenExistential existentialVar
    let boundA = substType (NamedVar boundName) (TVar existentialVar) typeA
    dirtyCtx <- isSubtypeOf existentialContext boundA typeB
    pure $ Ctx.dropEntriesUntilMarkerOf existentialVar dirtyCtx
  -- otherwise, fail!
  go a b =
    typeError
      [ "Type error."
      , ind [show a]
      , "is not a subtype of"
      , ind [show b]
      ]

-- | Instantiate the var to a subtype of the type
instantiateToSubtypeOf :: Var -> Tipe -> Ctx -> TC Ctx
instantiateToSubtypeOf = \v t ctx -> go ctx v t
 where
  go :: Ctx -> Var -> Tipe -> TC Ctx
  go baseCtx alpha = \case
    (TVar beta) -> do
      case Ctx.lookupBinding beta baseCtx of
        Nothing ->
          typeError ["Unbound variable (2)", ind1 beta]
        Just (Ctx.HasType termType) ->
          typeError
            [ "Cannot instantiate existential to term variable."
            , ind1 beta
            , "has type"
            , ind1 termType
            ]
        -- InstLReach
        Just (Ctx.IsExistential Nothing)
          | Ctx.definedBefore baseCtx alpha beta ->
              Ctx.solveExistential beta (Ctx.MonoVar alpha) baseCtx
        -- InstLSolve (via solved variable)
        Just (Ctx.IsExistential (Just solved)) ->
          -- Really this should not happen, bacause we use substCtxInType before
          -- each recursion
          Ctx.solveExistential alpha solved baseCtx
        -- InstLSolve (existential refers to outer universal or unsolved existential)
        Just _ ->
          Ctx.solveExistential alpha (Ctx.MonoVar beta) baseCtx
    -- InstLSolve (via unit-as-monotype)
    TUnit -> Ctx.solveExistential alpha Ctx.MonoUnit baseCtx
    -- InstLArr
    (TFunction argType retType) -> do
      argExists <- FreshVar.freshVar "InstLArr_arg"
      retExists <- FreshVar.freshVar "InstLArr_ret"
      CtxState.execCtx baseCtx do
        CtxState.articulateExistential
          alpha
          [retExists, argExists]
          (Ctx.MonoFunction (Ctx.MonoVar argExists) (Ctx.MonoVar retExists))
        CtxState.onCtx $ instantiateToSupertypeOf argType argExists
        retType' <- CtxState.getCtx <&> \ctx -> Ctx.substCtxInType ctx retType
        CtxState.onCtx $ instantiateToSubtypeOf retExists retType'
    -- InstLAIIR
    (TForall univName univType) -> CtxState.execCtx baseCtx do
      let univVar = NamedVar univName
      CtxState.bindUniversal univVar
      CtxState.onCtx $ instantiateToSubtypeOf alpha univType
      CtxState.dropEntriesUntilBinding_ univVar

-- | Instantiate the var to a supertype of the type
instantiateToSupertypeOf :: Tipe -> Var -> Ctx -> TC Ctx
instantiateToSupertypeOf = \t v ctx -> go ctx v t
 where
  go :: Ctx -> Var -> Tipe -> TC Ctx
  go baseCtx alpha = \case
    (TVar beta) -> case Ctx.lookupBinding beta baseCtx of
      Nothing ->
        typeError ["Unbound variable (1)", ind1 beta]
      Just (Ctx.HasType termType) ->
        typeError
          [ "Cannot instantiate existential to term variable."
          , ind1 beta
          , "has type"
          , ind1 termType
          ]
      -- InstRReach
      Just (Ctx.IsExistential Nothing)
        | Ctx.definedBefore baseCtx alpha beta ->
            Ctx.solveExistential beta (Ctx.MonoVar alpha) baseCtx
      -- InstRSolve (via solved variable)
      Just (Ctx.IsExistential (Just solved)) ->
        -- Really this should not happen, bacause we use substCtxInType before
        -- each recursion
        Ctx.solveExistential alpha solved baseCtx
      -- InstRSolve (existential refers to outer universal or unsolved existential)
      Just _ ->
        Ctx.solveExistential alpha (Ctx.MonoVar beta) baseCtx
    -- InstRSolve (via unit-as-monotype)
    TUnit -> Ctx.solveExistential alpha Ctx.MonoUnit baseCtx
    -- InstRArr
    TFunction argType retType -> CtxState.execCtx baseCtx do
      argExists <- FreshVar.freshVar "InstRArr_arg"
      retExists <- FreshVar.freshVar "InstRArr_ret"
      CtxState.articulateExistential
        alpha
        [retExists, argExists]
        (Ctx.MonoFunction (Ctx.MonoVar argExists) (Ctx.MonoVar retExists))
      CtxState.onCtx $ instantiateToSubtypeOf argExists argType
      retType' <- CtxState.getCtx <&> \ctx -> Ctx.substCtxInType ctx retType
      CtxState.onCtx $ instantiateToSupertypeOf retType' retExists
    -- InstRAIIL
    (TForall univName univType) -> CtxState.execCtx baseCtx do
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
      CtxState.markExistential univVar
      CtxState.bindOpenExistential univVar
      CtxState.onCtx $ instantiateToSupertypeOf univType alpha
      CtxState.dropEntriesUntilMarkerOf univVar

--------------------------------------------------------------------------------
-- Type checking

typeComplete :: Expr -> TC Tipe
typeComplete expr = do
  (tipe, finalCtx) <- CtxState.runCtx Ctx.emptyCtx (typeSynth expr)
  pure $ Ctx.substCtxInType finalCtx tipe

typeSynth :: Expr -> CtxState.CtxStateT TC Tipe
typeSynth = goSynth
 where
  goSynth :: Expr -> CtxState.CtxStateT TC Tipe
  goSynth = \case
    -- Var
    EVar varName ->
      CtxState.varIsBoundToATerm varName
    -- Anno
    EAnno expr tipe -> do
      CtxState.withCtx \ctx -> Ctx.isWellFormedType ctx tipe
      typeCheck expr tipe
      pure tipe
    -- ->E
    expr@(EApply callee argument) -> do
      calleeType <- do
        calleeType <- goSynth callee
        ctx <- CtxState.getCtx
        pure $ Ctx.substCtxInType ctx calleeType
      typeApply expr calleeType argument
    -- 1I⇒
    EUnit -> pure TUnit
    -- ->I⇒
    ELam argName body -> do
      let argVar = NamedVar argName
      alpha <- FreshVar.freshVar "->I⇒_arg"
      let argType = TVar alpha
      beta <- FreshVar.freshVar "->I⇒_ret"
      let retType = TVar beta
      CtxState.bindOpenExistential alpha
      CtxState.bindOpenExistential beta
      CtxState.bindTermVar argVar argType
      typeCheck body retType
      CtxState.dropEntriesUntilBinding_ argVar
      pure (TFunction argType retType)

typeCheck :: Expr -> Tipe -> CtxState.CtxStateT TC ()
typeCheck = goCheck
 where
  goCheck :: Expr -> Tipe -> CtxState.CtxStateT TC ()
  -- ForallI
  goCheck expr (TForall univName univType) = do
    let forallVar = NamedVar univName
    CtxState.bindUniversal forallVar
    goCheck expr univType
    CtxState.dropEntriesUntilBinding_ forallVar
  -- ->I
  goCheck (ELam argName body) (TFunction argType retType) = do
    let argVar = NamedVar argName
    CtxState.bindTermVar argVar argType
    goCheck body retType
    CtxState.dropEntriesUntilBinding_ argVar
  -- 1I
  goCheck EUnit TUnit = pure ()
  -- Sub
  goCheck expr expectedType = do
    foundType <- typeSynth expr
    CtxState.onCtx \ctx ->
      isSubtypeOf
        ctx
        (Ctx.substCtxInType ctx foundType)
        (Ctx.substCtxInType ctx expectedType)
        `catchTypeError` \err ->
          typeError
            [ err
            , "when typechecking expression"
            , ind [show expr]
            ]

typeApply :: Expr -> Tipe -> Expr -> CtxState.CtxStateT TC Tipe
typeApply overallApplyExpr = goApply
 where
  goApply :: Tipe -> Expr -> CtxState.CtxStateT TC Tipe
  -- ForallApp
  goApply (TForall univName univType) argument = do
    CtxState.bindOpenExistential (NamedVar univName)
    goApply univType argument
  -- ->App
  goApply (TFunction argType retType) argument = do
    typeCheck argument argType
    pure retType
  -- ExistApp
  goApply tipe@(TVar alpha) argument = do
    Ctx.lookupBinding alpha <$> CtxState.getCtx >>= \case
      Just (Ctx.IsExistential Nothing) -> do
        argVar <- FreshVar.freshVar "ExistApp_arg"
        retVar <- FreshVar.freshVar "ExistApp_ret"
        CtxState.articulateExistential
          alpha
          [retVar, argVar]
          (Ctx.MonoFunction (Ctx.MonoVar argVar) (Ctx.MonoVar retVar))
        typeCheck argument (TVar argVar)
        pure (TVar retVar)
      _ -> badApplyError tipe
  goApply tipe _ = badApplyError tipe
  badApplyError tipe =
    typeError $
      [ "Expected polymorphic or function type, but got"
      , ind [show tipe]
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

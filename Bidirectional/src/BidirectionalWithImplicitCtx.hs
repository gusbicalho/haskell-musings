{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module BidirectionalWithImplicitCtx where

import Bidirectional.Context qualified as Ctx
import Bidirectional.ContextState qualified as CtxState
import Bidirectional.FreshVar qualified as FreshVar
import Bidirectional.Language
import Bidirectional.ReportTypeErrors
import Control.Monad.Trans.Except (Except)
import Control.Monad.Trans.Except qualified as Except
import Bidirectional.Subtyping (isSubtypeOf)

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

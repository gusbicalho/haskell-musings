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
import Bidirectional.Subtyping (isSubtypeOf)
import Control.Monad.Trans.Except (Except)
import Control.Monad.Trans.Except qualified as Except

--------------------------------------------------------------------------------
-- Effects stack

type TC = CtxState.CtxStateT (FreshVar.FreshT (Except String))

runTC :: TC a -> Either String a
runTC = Except.runExcept . FreshVar.runFreshT . CtxState.evalCtx Ctx.emptyCtx

--------------------------------------------------------------------------------
-- Type checking

typeComplete :: Expr -> Either String Tipe
typeComplete expr = runTC do
  tipe <- typeSynth expr
  finalCtx <- CtxState.getCtx
  pure $ Ctx.substCtxInType finalCtx tipe

typeSynth :: Expr -> TC Tipe
typeSynth = goSynth
 where
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

typeCheck :: Expr -> Tipe -> TC ()
typeCheck = goCheck
 where
  -- ForallI
  goCheck expr (TForall forallVar univType) = do
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
    CtxState.onCtx (isSubtypeOf foundType expectedType)
      `catchTypeError` \err ->
        typeError
          [ err
          , "when typechecking expression"
          , ind [show expr]
          ]

typeApply :: Expr -> Tipe -> Expr -> TC Tipe
typeApply overallApplyExpr = goApply
 where
  -- ForallApp
  goApply (TForall forallVar univType) argument = do
    CtxState.bindOpenExistential forallVar
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
  go t@(TForall forallVar boundType)
    -- shadowing
    | forallVar == replacedVar = t
    -- no shadowing
    | otherwise = TForall forallVar (go boundType)

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- | Extended to infer polymorphic types for lambdas
module BidirectionalWithPolyLam where

import Bidirectional.Context qualified as Ctx
import Bidirectional.ContextState qualified as CtxState
import Bidirectional.FreshVar qualified as FreshVar
import Bidirectional.Language
import Bidirectional.ReportTypeErrors
import Bidirectional.Subtyping (isSubtypeOf)
import Control.Monad.Trans.Except (Except)
import Control.Monad.Trans.Except qualified as Except
import Data.Function ((&))
import Data.Maybe qualified as Maybe

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
  -- Instead of defaulting to Unit, we generalize the entire program like we
  -- did when synthesizing type for Lambda expressions. Each unsolved
  -- existential becomes a forall binder.
  unsolvedExistentials <- Ctx.unsolvedExistentials <$> CtxState.getCtx
  pure $ Ctx.substCtxInType finalCtx tipe `generalizedOver` unsolvedExistentials

generalizedOver :: Tipe -> [Var] -> Tipe
generalizedOver = foldr TForall

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
      -- The extension starts here
      -- In the original version, we create two existentials and then leave them
      -- in the context. These existentials may be solved when checking the body
      -- of the lambda, or they may be left unsolved, and may be solved later
      -- when checking the use sites for the lambda.
      -- In this extension, any existentials left unsolved after checking the
      -- body of the lambda are transformed into universals, by wrapping the
      -- inferred TFunction type into many TForalls.
      CtxState.markExistential alpha
      CtxState.bindOpenExistential alpha
      CtxState.bindOpenExistential beta
      CtxState.bindTermVar argVar argType
      typeCheck body retType
      funType <- Ctx.substCtxInType <$> CtxState.getCtx <*> pure (TFunction argType retType)
      (_, droppedEntries) <- CtxState.dropEntriesUntilMarkerOf alpha
      let unsolvedExistentials =
            droppedEntries & Maybe.mapMaybe \case
              Right (v, Ctx.IsExistential Nothing) -> Just v
              _ -> Nothing
      pure (funType `generalizedOver` unsolvedExistentials)

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

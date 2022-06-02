{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Bidirectional where

import Bidirectional.Context as Ctx
import Bidirectional.FreshVar qualified as FreshVar
import Bidirectional.Language
import Bidirectional.ReportTypeErrors
import Bidirectional.Subtyping (isSubtypeOf)
import Control.Monad.Trans.Except (Except)
import Control.Monad.Trans.Except qualified as Except
import Data.Functor ((<&>))

--------------------------------------------------------------------------------
-- Effects stack

type TC = FreshVar.FreshT (Except String)

runTC :: TC a -> Either String a
runTC = Except.runExcept . FreshVar.runFreshT

--------------------------------------------------------------------------------
-- Type checking

typeComplete :: Expr -> Either String Tipe
typeComplete expr = runTC do
  (ctx, tipe) <- typeSynth Ctx.emptyCtx expr
  -- The unsolved existential variables at this point are essentially "unused"
  -- types variables - values that are just passed around, there are no
  -- constraints on them. They are black boxes, so we can just set them to Unit.
  finalCtx <- Ctx.defaultAllUnsolvedExistentials MonoUnit ctx
  pure $ Ctx.substCtxInType finalCtx tipe

typeSynth :: Ctx.Ctx -> Expr -> TC (Ctx.Ctx, Tipe)
typeSynth ctx = goSynth
 where
  goSynth :: Expr -> TC (Ctx.Ctx, Tipe)
  goSynth = \case
    -- Var
    EVar varName -> (ctx,) <$> Ctx.varIsBoundToATerm ctx varName
    -- Anno
    EAnno expr tipe -> do
      Ctx.isWellFormedType ctx tipe
      ctx' <- typeCheck ctx expr tipe
      pure (ctx', tipe)
    -- ->E
    expr@(EApply callee argument) -> do
      (ctxAfterCallee, calleeType) <- goSynth callee
      typeApply expr ctxAfterCallee (Ctx.substCtxInType ctxAfterCallee calleeType) argument
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
          >>= Ctx.bindOpenExistential alpha
          >>= Ctx.bindOpenExistential beta
          >>= Ctx.bindTermVar argVar argType
      dirtyCtx <- typeCheck extendedCtx body retType
      pure
        ( Ctx.dropEntriesUntilBinding_ argVar dirtyCtx
        , TFunction argType retType
        )

typeCheck :: Ctx.Ctx -> Expr -> Tipe -> TC Ctx.Ctx
typeCheck ctx = goCheck
 where
  -- ForallI
  goCheck expr (TForall forallVar univType) = do
    extendedCtx <- Ctx.bindUniversal forallVar ctx
    dirtyCtx <- typeCheck extendedCtx expr univType
    pure (Ctx.dropEntriesUntilBinding_ forallVar dirtyCtx)
  -- ->I
  goCheck (ELam argName body) (TFunction argType retType) = do
    let argVar = NamedVar argName
    extendedCtx <- Ctx.bindTermVar argVar argType ctx
    dirtyCtx <- typeCheck extendedCtx body retType
    pure (Ctx.dropEntriesUntilBinding_ argVar dirtyCtx)
  -- 1I
  goCheck EUnit TUnit = pure ctx
  -- Sub
  goCheck expr expectedType = do
    (ctxAfterSynth, foundType) <- typeSynth ctx expr
    isSubtypeOf foundType expectedType ctxAfterSynth
      `catchTypeError` \err ->
        typeError
          [ err
          , "when typechecking expression"
          , ind [show expr]
          ]

typeApply :: Expr -> Ctx.Ctx -> Tipe -> Expr -> TC (Ctx.Ctx, Tipe)
typeApply overallApplyExpr = goApply
 where
  -- ForallApp
  goApply ctx (TForall forallVar univType) argument = do
    extendedCtx <- Ctx.bindOpenExistential forallVar ctx
    goApply extendedCtx univType argument
  -- ->App
  goApply ctx (TFunction argType retType) argument = do
    typeCheck ctx argument argType <&> (,retType)
  -- ExistApp
  goApply ctx (TVar alpha) argument
    | Just (Ctx.IsExistential Nothing) <- Ctx.lookupBinding alpha ctx = do
        argVar <- FreshVar.freshVar "ExistApp_arg"
        retVar <- FreshVar.freshVar "ExistApp_ret"
        articulatedCtx <-
          Ctx.articulateExistential
            alpha
            [retVar, argVar]
            (Ctx.MonoFunction (Ctx.MonoVar argVar) (Ctx.MonoVar retVar))
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
  go t@(TForall forallVar boundType)
    -- shadowing
    | forallVar == replacedVar = t
    -- no shadowing
    | otherwise = TForall forallVar (go boundType)

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Bidirectional where

import Bidirectional.Context
import Bidirectional.FreshVar qualified as FreshVar
import Bidirectional.Language
import Bidirectional.ReportTypeErrors
import Bidirectional.Subtyping (isSubtypeOf)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except qualified as Except
import Data.Functor ((<&>))

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

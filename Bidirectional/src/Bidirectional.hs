{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Bidirectional where

import Control.Monad (unless)
import Control.Monad.Trans.Class qualified as Trans
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except qualified as Except
import Control.Monad.Trans.State.Strict (State)
import Control.Monad.Trans.State.Strict qualified as State
import Data.List qualified as List

data Var
  = NamedVar String
  | FreshVar Word
  deriving stock (Eq, Ord, Show)

data Expr
  = EUnit
  | EVar Var
  | EAnno Expr Tipe
  | ELam String Expr
  | EApply Expr Expr
  deriving stock (Eq, Ord, Show)

data Tipe
  = TUnit
  | TVar Var
  | TFunction Tipe Tipe
  | TForall String Tipe
  deriving stock (Eq, Ord, Show)

data CtxEntry
  = IsType
  | HasType Tipe

type Ctx = [(Var, CtxEntry)]

--------------------------------------------------------------------------------
-- Well-formedness of types and subtyping

isWellFormedType :: Ctx -> Tipe -> TC ()
isWellFormedType ctx = \case
  -- DeclUnitWF
  TUnit -> pure ()
  -- DeclUvarWF
  (TVar var) -> varIsBoundToAType ctx var
  -- DeclArrowWF
  (TFunction argType retType) -> do
    isWellFormedType ctx argType
    isWellFormedType ctx retType
  -- DeclForallWF
  TForall boundName boundType ->
    isWellFormedType ((NamedVar boundName, IsType) : ctx) boundType

isSubtypeOf :: Ctx -> Tipe -> Tipe -> TC ()
isSubtypeOf ctx = go
 where
  -- <=Unit
  go TUnit TUnit = pure ()
  -- <=Var
  go (TVar v1) (TVar v2) = do
    varIsBoundToAType ctx v1
    unless (v1 == v2) do
      varIsBoundToAType ctx v2 -- better error msg if v2 if unbound
      typeError ["Type error.", ind [show v1], "is not the same as", ind [show v2]]
  -- <=->
  go (TFunction argA retA) (TFunction argB retB) = do
    isSubtypeOf ctx argB argA
    isSubtypeOf ctx retA retB
  -- "In fact, the rules are practically syntax-directed: the only exception is
  --  when both types are quantifiers, and either ≤∀L or ≤∀R could be tried.
  --  Since ≤∀R is invertible, however, in practice one can apply it eagerly"
  -- <=ForallR
  go typeA (TForall boundName boundType) = do
    isSubtypeOf ((NamedVar boundName, IsType) : ctx) typeA boundType
  -- <=ForallL
  go a@(TForall boundName typeA) typeB = do
    boundType <- TVar <$> freshTypeVar -- TODO: guess some monotype!
    let boundA = substType (NamedVar boundName) boundType typeA
    unless (boundA == typeB) do
      typeError
        [ "Type error."
        , ind [show a]
        , "is not a subtype of"
        , ind [show typeB]
        ]
  -- otherwise, fail!
  go a b =
    typeError
      [ "Type error."
      , ind [show a]
      , "is not a subtype of"
      , ind [show b]
      ]

varIsBoundToAType :: Ctx -> Var -> TC ()
varIsBoundToAType ctx var =
  case lookup var ctx of
    Just IsType -> pure ()
    Just (HasType termType) ->
      typeError
        [ "Expected a type, but"
        , ind [show var]
        , "is a term of type"
        , ind [show termType]
        ]
    Nothing ->
      typeError ["Unbound var", ind [show var]]

varIsBoundToATerm :: Ctx -> Var -> TC Tipe
varIsBoundToATerm ctx var =
  case lookup var ctx of
    Just (HasType termType) -> pure termType
    Just IsType ->
      typeError
        [ "Expected a term, but"
        , ind [show var]
        , "is a Type."
        ]
    Nothing ->
      typeError ["Unbound var", ind [show var]]

--------------------------------------------------------------------------------
-- Type checking

typeSynth :: Ctx -> Expr -> TC Tipe
typeSynth ctx = goSynth
 where
  goSynth = \case
    -- DeclVar
    EVar varName -> varIsBoundToATerm ctx varName
    -- DeclAnno
    EAnno expr tipe -> do
      isWellFormedType ctx tipe
      typeCheck ctx expr tipe
      pure tipe
    -- Decl->E
    expr@(EApply callee argument) -> do
      calleeType <- goSynth callee
      resultType <- typeApply ctx expr calleeType argument
      pure resultType
    -- "To show that our system can accommodate these kinds of extensions, we
    --  add the Decl1I⇒ and Decl→I⇒ rules, which synthesize a unit type for ()
    --  and a monomorphic function type for lambda expressions λx."
    -- Decl1I⇒
    EUnit -> pure TUnit
    -- Decl->I⇒
    ELam argName body -> do
      -- TODO infer 2 monotypes
      argTypeVar <- freshTypeVar
      let argType = TVar argTypeVar
      retTypeVar <- freshTypeVar
      let retType = TVar retTypeVar
      let extendedCtx = [(argTypeVar, IsType), (retTypeVar, IsType)] <> ctx
      -- TODO: This is wrong, but we have no room for unifying the two fresh
      -- variables. However, let us leave this here for now, otherwise things
      -- will fail due to the fresh vars being unbound
      isWellFormedType extendedCtx (TFunction argType retType)
      typeCheck ((NamedVar argName, HasType argType) : extendedCtx) body retType
      pure $ TFunction argType retType

typeCheck :: Ctx -> Expr -> Tipe -> TC ()
typeCheck ctx = goCheck
 where
  -- DeclForallI
  goCheck expr (TForall typeVarName tipe) =
    typeCheck ((NamedVar typeVarName, IsType) : ctx) expr tipe
  -- Decl->I
  goCheck (ELam argName body) (TFunction argType retType) =
    typeCheck ((NamedVar argName, HasType argType) : ctx) body retType
  -- Decl1I
  goCheck EUnit TUnit = pure ()
  -- DeclSub
  goCheck expr expectedType = do
    foundType <- typeSynth ctx expr
    isSubtypeOf ctx foundType expectedType
      `Except.catchE` \err ->
        typeError
          [ err
          , "when typechecking expression"
          , ind [show expr]
          ]

typeApply :: Ctx -> Expr -> Tipe -> Expr -> TC Tipe
typeApply ctx applyExpr = goApply
 where
  -- DeclForallApp
  goApply (TForall typeVarName boundType) argument = do
    argType <- TVar <$> freshTypeVar
    goApply (substType (NamedVar typeVarName) argType boundType) argument
  -- Decl->App
  goApply (TFunction argType retType) argument = do
    typeCheck ctx argument argType
    pure retType
  goApply other _ =
    typeError $
      [ "Expected polymorphic or function type, but got"
      , ind [show other]
      , "in application expression"
      , ind [show applyExpr]
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

-- Effects stack
type TC a = ExceptT String (State Word) a

runTC :: TC a -> Either String a
runTC = flip State.evalState 0 . Except.runExceptT

-- fresh type var

freshTypeVar :: TC Var
freshTypeVar = do
  fresh <- Trans.lift State.get
  Trans.lift $ State.modify' succ
  pure $ FreshVar fresh

-- Error reporting

typeError :: [String] -> TC a
typeError = Except.throwE . List.intercalate "\n"

ind :: [String] -> String
ind = List.intercalate "\n" . fmap ("  " <>)

expectRight :: TC a -> a
expectRight = either error id . runTC

orFailWith :: Maybe a -> [String] -> TC a
mb `orFailWith` err =
  case mb of
    Just a -> pure a
    Nothing -> typeError err

--------------------------------------------------------------------------------
-- Example

test_id :: Expr
test_id = ELam "x" (EVar (NamedVar "x"))

-- >>> expectRight . typeSynth [] $ test_id
-- Type error.
--   FreshVar 0
-- is not the same as
--   FreshVar 1
-- in expression
--   EVar (NamedVar "x")

-- >>> expectRight $ typeCheck [(NamedVar "T", IsType)] test_id (TFunction (TVar (NamedVar "T")) (TVar (NamedVar "T")))
-- ()

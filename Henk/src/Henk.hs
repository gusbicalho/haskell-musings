{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DerivingStrategies #-}

module Henk where

import Data.Foldable qualified as F
import Data.Kind qualified
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Proxy (Proxy (Proxy))
import Data.String (IsString (fromString))
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits (KnownSymbol, symbolVal)

--------------------------------------------------------------------------------
-- Syntax

data Program id lit
  = MkProgram [TypeDeclaration id lit] [ValueDeclaration id lit]
  deriving stock (Eq, Ord, Show)

data TypeDeclaration id lit
  = TypedVariable id lit ::= (NonEmpty (TypedVariable id lit))
  deriving stock (Eq, Ord, Show)

data ValueDeclaration id lit
  = Let (Binding id lit)
  | Letrec (NonEmpty (Binding id lit))
  deriving stock (Eq, Ord, Show)

data Binding id lit
  = TypedVariable id lit := Expression id lit
  deriving stock (Eq, Ord, Show)

data Expression id lit
  = ELookup (TypedVariable id lit)
  | ELiteral lit
  | EType
  | EKind
  | EApply (Expression id lit) (Expression id lit)
  | ELambda (NonEmpty (TypedVariable id lit)) (Expression id lit)
  | EPi (NonEmpty (TypedVariable id lit)) (Expression id lit)
  | ValueDeclaration id lit `EIn` Expression id lit
  | ECase (Expression id lit) (NonEmpty (CaseAlternative id lit)) [Expression id lit]
  deriving stock (Eq, Ord, Show)

data CaseAlternative id lit
  = CasePattern id lit :=> Expression id lit
  deriving stock (Eq, Ord, Show)

data CasePattern id lit
  = CPVariable (TypedVariable id lit)
  | CPLiteral lit
  deriving stock (Eq, Ord, Show)

data TypedVariable id lit
  = Variable id :~ Expression id lit
  deriving stock (Eq, Ord, Show)

data Variable id
  = Ignore
  | Var id
  deriving stock (Eq, Ord, Show)

-- Small piece of sugar that has to live here to avoid orphan instance
instance (KnownSymbol label, IsString id) => IsLabel label (Variable id) where
  fromLabel = case symbolVal (Proxy @label) of
    "_" -> Ignore
    other -> Var (fromString other)

--------------------------------------------------------------------------------
-- Type checking

type Result a = Either String a

type Ctx id lit = [(id, Expression id lit)]

checkProgram :: (Eq id, Show id, Show lit) => Ctx id lit -> Program id lit -> Result (Ctx id lit)
checkProgram baseCtx (MkProgram typedecls valuedecls) =
  pure baseCtx
    >>= checkTypes
    >>= checkValues
 where
  checkTypes ctx =
    F.foldlM
      (extendingCtx checkTypeDeclaration)
      ctx
      typedecls
  checkValues ctx =
    F.foldlM
      (extendingCtx checkValueDeclaration)
      ctx
      valuedecls
  extendingCtx f accum v = (<> accum) <$> f accum v

checkTypeDeclaration :: (Eq id, Show id, Show lit) => Ctx id lit -> TypeDeclaration id lit -> Result (Ctx id lit)
checkTypeDeclaration ctx typedecl@(typename :~ typekind ::= constructors) = do
  typeId <- case typename of
    Ignore -> Left $ "Top-level declarations must have an identifier, missing at:\n" <> show typedecl
    Var id -> pure id
  let constructorsCtx = []
  -- TODO check
  pure $ (typeId, typekind) : constructorsCtx

checkValueDeclaration :: (Eq id, Show id, Show lit) => Ctx id lit -> ValueDeclaration id lit -> Result (Ctx id lit)
checkValueDeclaration = _

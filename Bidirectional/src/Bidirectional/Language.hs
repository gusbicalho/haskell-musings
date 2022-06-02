{-# LANGUAGE DerivingStrategies #-}
module Bidirectional.Language where

data Var
  = NamedVar String
  | FreshVar String Word
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
  | TForall Var Tipe
  deriving stock (Eq, Ord, Show)

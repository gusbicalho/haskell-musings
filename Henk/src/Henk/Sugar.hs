{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Henk.Sugar where

import Data.Kind qualified
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy (Proxy (Proxy))
import GHC.OverloadedLabels (IsLabel (fromLabel))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Henk

-- | Helper to hang sugar instances on literals
newtype Literal (id :: Data.Kind.Type) lit = Lit lit

lit :: lit -> Literal id lit
lit = Lit

-- | Convert stuff to Expression
class ToExpr x id lit | x -> id lit where
  toExpr :: x -> Expression id lit

instance ToExpr (Expression id lit) id lit where
  toExpr = id

instance ToExpr (TypedVariable id lit) id lit where
  toExpr = ELookup

instance ToExpr (Literal id lit) id lit where
  toExpr (Lit e) = ELiteral e

-- Sugar

letrec :: NonEmpty (Binding id lit) -> ValueDeclaration id lit
letrec = Letrec

(~:) :: ToExpr x id lit => Variable id -> x -> TypedVariable id lit
e ~: t = e :~ toExpr t

pi' :: ToExpr x id lit => NonEmpty (TypedVariable id lit) -> x -> Expression id lit
pi' vts u = case toExpr u of
  EPi args expr -> EPi (vts <> args) expr
  u_expr -> EPi vts u_expr

(~>) :: (ToExpr x1 id lit, ToExpr x2 id lit) => x2 -> x1 -> Expression id lit
t ~> u = pi' [Ignore ~: t] u

(-->) :: ToExpr x id lit => TypedVariable id lit -> x -> Expression id lit
v --> e = ELambda [v] (toExpr e)

(!) :: (ToExpr x1 id lit, ToExpr x2 id lit) => x1 -> x2 -> Expression id lit
f ! a = EApply (toExpr f) (toExpr a)

class FromBinding v where
  fromBinding :: Binding id lit -> v id lit

instance FromBinding Binding where
  fromBinding = id

instance FromBinding ValueDeclaration where
  fromBinding = Let

(=:) :: (FromBinding v, ToExpr x id lit) => TypedVariable id lit -> x -> v id lit
v =: expr = fromBinding (v := toExpr expr)

class ToCaseAlternative pat id lit | pat -> id lit where
  (==>) :: ToExpr exp id lit => pat -> exp -> CaseAlternative id lit

instance ToCaseAlternative (CasePattern id lit) id lit where
  pat ==> e = pat :=> toExpr e

instance ToCaseAlternative (TypedVariable id lit) id lit where
  pat ==> e = CPVariable pat :=> toExpr e

instance ToCaseAlternative (Literal id lit) id lit where
  Lit pat ==> e = CPLiteral pat :=> toExpr e

infixl 5 !
infixr 4 ~>
infixr 4 -->
infixl 3 ~:
infixr 2 ==>
infixl 2 =:

-- Sugar for Case
data CaseOfArgs id lit = CaseOf (Expression id lit) (NonEmpty (CaseAlternative id lit))

caseOf ::
  ToExpr x id lit =>
  x ->
  (NonEmpty (CaseAlternative id lit)) ->
  CaseOfArgs id lit
e `caseOf` cases = CaseOf (toExpr e) cases

at :: CaseOfArgs id lit -> [Expression id lit] -> Expression id lit
CaseOf arg cases `at` atClause = ECase arg cases atClause

infix 6 `at`

instance ToExpr (CaseOfArgs id lit) id lit where
  toExpr = (`at` [])

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Henk.PTS.Fw where

import Henk.PTS.Core

data Ko = KIND | TYPE
  deriving stock (Eq, Ord, Show)

type E = Expr Ko String

type Ctx = [(String, E)]

kAxioms :: Axioms Ko
kAxioms = \case
  TYPE -> Just KIND
  KIND -> Nothing

(~~>) :: AbstractionRules Ko
KIND ~~> t = Just t -- Types can be bound in Terms or in Types
TYPE ~~> TYPE = Just TYPE -- Terms can be bound in Terms
TYPE ~~> KIND = Nothing -- Terms CANNOT be bound in Types

fw :: Ctx -> E -> Either String E
fw = inferType kAxioms (~~>)

expectRight :: Either String c -> c
expectRight = either error id

{-
>>> minCtx = [("Unit", Var "UNIT"), ("UNIT", K TYPE)]

>>> expectRight . fw minCtx $ lam "x" (Var "UNIT") (Var "x")
Pi (MkBinding {boundVar = "x", bindingType = Var "UNIT", boundExpr = Var "UNIT"})

>>> expectRight . fw minCtx $ lam "x" (Var "UNIT") (Var "x") `app` Var "Unit"
Var "UNIT"

>>> expectRight . fw minCtx $ lam "T" (K TYPE) (lam "x" (Var "T") (Var "x"))
Pi (MkBinding {boundVar = "T", bindingType = K TYPE, boundExpr = Pi (MkBinding {boundVar = "x", bindingType = Var "T", boundExpr = Var "T"})})

>>> expectRight . fw minCtx $ lam "T" (K TYPE) (lam "x" (Var "T") (Var "x")) `app` Var "UNIT"
Pi (MkBinding {boundVar = "x", bindingType = Var "UNIT", boundExpr = Var "UNIT"})

>>> expectRight . fw minCtx $ lam "T" (K TYPE) (lam "x" (Var "T") (Var "x")) `app` Var "Unit"
Bad argument. Expected a
  K TYPE
but got
  Var "Unit"
which is a
  Var "UNIT"

>>> expectRight . fw minCtx $ lam "T" (K TYPE) (lam "x" (Var "T") (Var "x")) `app` K TYPE
Bad argument. Expected a
  K TYPE
but got
  K TYPE
which is a
  K KIND

>>> expectRight . fw minCtx $ lam "unit" (Var "UNIT") (lam "T" (K TYPE) (lam "x" (Var "T") (Var "x")) `app` Var "unit")
Bad argument. Expected a
  K TYPE
but got
  Var "unit"
which is a
  Var "UNIT"

>>> expectRight . fw minCtx $ lam "unit" (Var "UNIT") $ Var "UNIT"
Pi binding not allowed: Var "UNIT" : TYPE ~~> K TYPE : KIND

-}

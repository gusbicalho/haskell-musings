module Henk.PTS.SimplyTyped where

import Prelude hiding (pi)
import Henk.PTS

data Ko = TYPE deriving (Eq, Show)

simplyTyped ::
  [(String, Expr Ko String)] ->
  Expr Ko String ->
  Either String (Expr Ko String)
simplyTyped =
  inferType @Ko @String @[(String, Expr _ String)]
    (\_ -> Nothing)
    (\_ _ -> Just TYPE)

expectRight :: Either String c -> c
expectRight = either error id

{-
>>> minCtx = [("unit", Var "UNIT"), ("UNIT", K TYPE)]

>>> expectRight . simplyTyped minCtx $ lam "x" (Var "UNIT") (Var "x")
Pi (MkBinding {boundVar = "x", bindingType = Var "UNIT", boundExpr = Var "UNIT"})

>>> expectRight . simplyTyped minCtx $ lam "f" (pi "_" (Var "UNIT") (Var "UNIT")) $ lam "x" (Var "UNIT") $ (Var "f") `app` (Var "x")
Pi (MkBinding {boundVar = "f", bindingType = Pi (MkBinding {boundVar = "_", bindingType = Var "UNIT", boundExpr = Var "UNIT"}), boundExpr = Pi (MkBinding {boundVar = "x", bindingType = Var "UNIT", boundExpr = Var "UNIT"})})

-}

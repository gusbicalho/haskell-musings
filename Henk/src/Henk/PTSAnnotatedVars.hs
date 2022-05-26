{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Henk.PTSAnnotatedVars where

import Henk.PTS (Binding (..), Expr (..))

data K = KIND | TYPE | UNIT | Unit
  deriving stock (Eq, Ord, Show)

-- Unit : UNIT
-- UNIT : TYPE
-- TYPE : KIND

data V = String :~ E
  deriving stock (Eq, Ord, Show)

type E = Expr K V

(~~>) :: K -> K -> Bool
TYPE ~~> TYPE = True -- Terms can be bound in Terms
KIND ~~> TYPE = True -- Types can be bound in Terms
KIND ~~> KIND = True -- Types can be bound in Types
TYPE ~~> KIND = False -- Terms CANNOT be bound in Types
_ ~~> _ = False -- Pi types relating other entities do not make sense

inferType0 :: E -> E
inferType0 e = case inferType e of
  Right t -> t
  Left err -> error err

inferType :: E -> Either String E
inferType = \case
  K Unit -> pure $ K UNIT
  K UNIT -> pure $ K TYPE
  K TYPE -> pure $ K KIND
  K KIND -> Left "KIND does not have a type"
  Var (_ :~ t) -> pure t
  Apply apply argument -> do
    apply_type <- betaReduce <$> inferType apply
    pure $ betaReduce (Apply apply_type argument)
  Lambda MkBinding{boundVar, bindingType, boundExpr} -> do
    return_type <- inferType boundExpr
    let pi_type =
          Pi
            MkBinding
              { boundVar
              , bindingType
              , boundExpr = return_type
              }
    pure pi_type
  Pi MkBinding{boundExpr} -> do
    resultType_kind <-
      betaReduce <$> inferType boundExpr >>= \case
        K k -> pure k
        other -> Left $ "Unable to find Pi result type kind in WHNF:\n  " <> show other
    pure (K resultType_kind)

betaReduce :: E -> E
betaReduce = \case
  e@K{} -> e
  e@Lambda{} -> e
  e@Pi{} -> e
  e@Var{} -> e
  Apply apply argument ->
    let apply' = betaReduce apply
        argument' = betaReduce argument
     in case apply' of
          Lambda binding -> betaReduce $ bind binding argument'
          Pi binding -> betaReduce $ bind binding argument'
          _ -> Apply apply' argument'

bind :: Binding K V -> E -> E
bind MkBinding{boundVar, boundExpr} argument =
  subst boundVar argument boundExpr

subst :: V -> E -> E -> E
subst var value = go
 where
  go = \case
    e@(K _) -> e
    e@(Var v)
      | v == var -> value
      | otherwise -> e
    Pi binding -> Pi (substInBinding binding)
    Lambda binding -> Lambda (substInBinding binding)
    Apply apply argument -> Apply (go apply) (go argument)
  substInBinding binding@MkBinding{bindingType, boundExpr} =
    binding
      { bindingType = go bindingType
      , boundExpr = go boundExpr
      }

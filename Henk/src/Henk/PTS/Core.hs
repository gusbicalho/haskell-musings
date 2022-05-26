{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}

module Henk.PTS.Core where

import Control.Monad (when)
import Prelude hiding (pi)

-- Syntax

data Expr k var
  = K k
  | Var var
  | Apply (Expr k var) (Expr k var)
  | Lambda (Binding k var)
  | Pi (Binding k var)
  deriving stock (Eq, Ord, Show)

data Binding k var = MkBinding
  { boundVar :: var
  , bindingType :: Expr k var
  , boundExpr :: Expr k var
  }
  deriving stock (Eq, Ord, Show)

-- Syntax helpers

app :: Expr k var -> Expr k var -> Expr k var
app = Apply

lam :: var -> Expr k var -> Expr k var -> Expr k var
lam v t body = Lambda (MkBinding v t body)

pi :: var -> Expr k var -> Expr k var -> Expr k var
pi v t body = Pi (MkBinding v t body)

-- Type system extension points

type Axioms k = k -> Maybe k
type AbstractionRules k = k -> k -> Maybe k

-- Type inference/checking

newtype HasType t = MkHasType t

class Context ctx k var | ctx -> k var where
  ctxLookup :: var -> ctx -> Maybe (HasType (Expr k var))
  ctxExtend :: var -> Expr k var -> ctx -> ctx

instance Eq var => Context [(var, Expr k var)] k var where
  ctxLookup v = fmap MkHasType . lookup v
  ctxExtend var expr = ((var, expr) :)

inferType ::
  forall k var ctx.
  (Context ctx k var, Show k, Show var, Eq var, Eq k) =>
  Axioms k ->
  AbstractionRules k ->
  ctx ->
  Expr k var ->
  Either String (Expr k var)
inferType kAxioms (~~>) = go
 where
  go :: ctx -> Expr k var -> Either String (Expr k var)
  go ctx = \case
    K k -> case kAxioms k of
      Just k_type -> pure (K k_type)
      Nothing -> Left $ show k <> " does not have a type"
    Var var -> case ctxLookup var ctx of
      Nothing -> Left $ "Free var " <> show var
      Just (MkHasType e) -> pure e
    Apply apply argument -> do
      apply_type <- betaReduce <$> go ctx apply
      case apply_type of
        Pi binding@MkBinding{bindingType} -> do
          argument_type <- betaReduce <$> go ctx argument
          when (not $ isWHNF argument_type) $
            Left $ "Unable to reduce argument type to WHNF:\n  " <> show argument_type
          when (bindingType /= argument_type) $
            Left $
              "Bad argument. Expected a\n  "
                <> show bindingType
                <> "\nbut got\n  "
                <> show argument
                <> "\nwhich is a\n  "
                <> show argument_type
          pure $ bind binding argument
        other -> Left $ "Non-PI type in application: " <> show other
    Lambda MkBinding{boundVar, bindingType, boundExpr} -> do
      return_type <- go (ctxExtend boundVar bindingType ctx) boundExpr
      let pi_type =
            Pi
              MkBinding
                { boundVar
                , bindingType
                , boundExpr = return_type
                }
      -- check that the Pi type is well sorted
      _ <- go ctx pi_type
      pure pi_type
    Pi MkBinding{boundVar, bindingType, boundExpr} -> do
      bindingType_kind <-
        betaReduce <$> go ctx bindingType >>= \case
          K k -> pure k
          other -> Left $ "Unable to find Pi binding type kind in WHNF:\n  " <> show other
      resultType_kind <-
        betaReduce <$> go (ctxExtend boundVar bindingType ctx) boundExpr >>= \case
          K k -> pure k
          other -> Left $ "Unable to find Pi result type kind in WHNF:\n  " <> show other
      case bindingType_kind ~~> resultType_kind of
        Just piType_type -> pure (K piType_type)
        Nothing ->
          Left $
            "Pi binding not allowed: "
              <> (show bindingType <> " : " <> show bindingType_kind)
              <> " ~~> "
              <> (show boundExpr <> " : " <> show resultType_kind)

isWHNF :: Expr k var -> Bool
isWHNF Apply{} = False
isWHNF _ = True

betaReduce :: Eq var => Expr k var -> Expr k var
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

bind :: Eq p => Binding k p -> Expr k p -> Expr k p
bind MkBinding{boundVar, boundExpr} argument =
  subst boundVar argument boundExpr

subst :: Eq p => p -> Expr k p -> Expr k p -> Expr k p
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

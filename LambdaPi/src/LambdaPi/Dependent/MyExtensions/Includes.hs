{-# LANGUAGE LambdaCase #-}

module LambdaPi.Dependent.MyExtensions.Includes (
  Includes (..),
) where

import Data.Kind (Constraint, Type)

type Includes :: Type -> Type -> Constraint
class Includes larger smaller where
  inject :: smaller -> larger
  project :: larger -> Maybe smaller

instance Includes a a where
  inject = id
  project = Just

instance Includes (Either a b) a where
  inject = Left
  project = \case
    Left a -> Just a
    _ -> Nothing

instance Includes (Either a b) b where
  inject = Right
  project = \case
    Right b -> Just b
    _ -> Nothing

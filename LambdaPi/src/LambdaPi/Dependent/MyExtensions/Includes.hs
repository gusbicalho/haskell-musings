{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module LambdaPi.Dependent.MyExtensions.Includes (
  Includes (..),
  IncludesSelf (..),
) where

import Data.Kind (Constraint, Type)

type Includes :: Type -> Type -> Constraint
class Includes larger smaller where
  inject :: smaller -> larger
  project :: larger -> Maybe smaller

-- | Helper for deriving via
newtype IncludesSelf a = MkIncludesSelf a

instance Includes a (IncludesSelf a) where
  inject (MkIncludesSelf a) = a
  project = Just . MkIncludesSelf

deriving via (IncludesSelf (Either a b)) instance Includes (Either a b) (Either a b)
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

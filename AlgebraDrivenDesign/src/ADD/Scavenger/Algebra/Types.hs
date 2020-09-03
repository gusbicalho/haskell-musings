{-# LANGUAGE DerivingVia #-}
module ADD.Scavenger.Algebra.Types where

import Data.Semigroup (Max(Max))
data Challenge i k r

data InputFilter i

data ClueState
  deriving (Eq, Ord)
  deriving Semigroup via Max ClueState

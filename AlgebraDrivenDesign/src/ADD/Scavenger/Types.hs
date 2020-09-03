{-# LANGUAGE DerivingVia #-}
module ADD.Scavenger.Types where

import Data.Semigroup (Max(Max))
data Challenge i k r

data Input

data InputFilter i

data Reward deriving (Eq, Ord)

data Clue deriving (Eq, Ord)

data ClueState
  deriving (Eq, Ord)
  deriving Semigroup via Max ClueState

data Point

data Distance

data Altitude

data Photo

data Time

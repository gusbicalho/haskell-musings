{-# LANGUAGE DerivingVia #-}

module ADD.Scavenger.Algebra2.ClueState where

import Data.Semigroup (Max (Max))

data ClueState = Seen | Failed | Completed
  deriving (Eq, Ord, Show, Enum, Bounded)
  deriving (Semigroup, Monoid) via Max ClueState

seen :: ClueState
seen = Seen

completed :: ClueState
completed = Completed

failed :: ClueState
failed = Failed

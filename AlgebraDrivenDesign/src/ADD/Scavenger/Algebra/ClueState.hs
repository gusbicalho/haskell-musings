{-# LANGUAGE DerivingVia #-}

module ADD.Scavenger.Algebra.ClueState where

import Data.Semigroup (Max (Max))

data ClueState
  deriving (Eq, Ord)
  deriving (Semigroup) via Max ClueState

seen :: ClueState
seen = undefined

completed :: ClueState
completed = undefined

failed :: ClueState
failed = undefined

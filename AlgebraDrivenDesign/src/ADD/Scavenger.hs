{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module ADD.Scavenger where

import ADD.Scavenger.Algebra.Challenge
  ( Challenge,
    ChallengeOutput,
    Commutative,
    clue,
    eitherC,
    empty,
    gate,
    pumpChallenge,
    reward,
  )
import ADD.Scavenger.Algebra.InputFilter
  ( HasFilter (..),
    InputFilter,
  )
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet

-- Input-related types
data Input

data Point

data Distance

data Altitude

data Photo

data Time

instance HasFilter Input where
  type CustomFilter Input = Input -> Bool
  filterMatches f i = f i

-- Other types
newtype Hint = Hint String
  deriving (Eq, Ord)

data Reward deriving (Eq, Ord)

newtype Rewards = Rewards {getRewards :: MultiSet Reward}
  deriving newtype (Semigroup, Monoid)

instance Commutative Rewards

-- Challenges

runGame :: [Input] -> (ChallengeOutput Hint Rewards, Challenge Input Hint Rewards)
runGame =
  pumpChallenge
    $ clue [Hint "Run around the block in under five minutes"]
    $ timeout fiveMinutes
    $ aroundTheBlock
  where
    aroundTheBlock =
      gate (locWithin p1 tolerance)
        $ gate (locWithin p2 tolerance)
        $ gate (locWithin p3 tolerance)
        $ gate (locWithin p4 tolerance)
        $ reward tenPoints
    fiveMinutes = undefined :: Time
    p1 = undefined :: Point
    p2 = undefined :: Point
    p3 = undefined :: Point
    p4 = undefined :: Point
    tolerance = undefined :: Distance
    tenPoints = Rewards (MultiSet.singleton undefined)

timeout :: Time -> Challenge Input k r -> Challenge Input k r
timeout t c = eitherC (gate (afterTime t) empty) c

pointOfInterest :: [k] -> Point -> Distance -> r -> Challenge Input k r
pointOfInterest k p d r = clue k (gate (photoWithin p d) (reward r))

-- Filters
locWithin :: Point -> Distance -> InputFilter Input
locWithin _ _ = undefined

photoWithin :: Point -> Distance -> InputFilter Input
photoWithin _ _ = undefined

photoAbove :: Altitude -> InputFilter Input
photoAbove _ = undefined

afterTime :: Time -> InputFilter Input
afterTime _ = undefined

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module ADD.Scavenger where

import ADD.Scavenger.Algebra.Challenge
  ( Challenge,
    Commutative,
    Results,
    andThen,
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
    andF,
    custom,
  )
import Data.Semigroup (Sum (Sum))
import Numeric.Natural (Natural)

-- Input-related types
data Input
  = EventTime Time
  | EventLocation Point
  | EventPhoto Point Altitude Photo

data Photo = Photo
  deriving (Eq, Ord, Show)

data Point = Point Double Double
  deriving (Eq, Ord, Show)

data Distance = Distance Double
  deriving (Eq, Ord, Show)

data Altitude = Altitude Double
  deriving (Eq, Ord, Show)

data Time = Time Natural
  deriving (Eq, Ord, Show)

instance HasFilter Input where
  data CustomFilter Input
    = IsLocation
    | IsPhoto
    | AfterTime Time
    | Within Point Distance
    | Above Altitude
    deriving (Eq, Show)
  filterMatches IsLocation (EventLocation {}) = True
  filterMatches IsPhoto (EventPhoto {}) = True
  filterMatches (AfterTime t0) (EventTime te) = te > t0
  filterMatches (Within target (Distance d)) i
    | EventLocation loc <- i = squaredDistance loc target <= d * d
    | EventPhoto loc _ _ <- i = squaredDistance loc target <= d * d
  filterMatches (Above threshold) i
    | EventPhoto _ altitude _ <- i = altitude > threshold
  filterMatches _ _ = False

-- Other types
newtype Hint = Hint String
  deriving (Eq, Ord, Show)

newtype Rewards = Points Integer
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num)
  deriving (Semigroup, Monoid) via (Sum Rewards)

instance Commutative Rewards

-- Challenges

runGame :: [Input] -> (Results Hint Rewards, Challenge Input Hint Rewards)
runGame =
  pumpChallenge
    $ timeout fiveMinutes
    $ clue [Hint "Run around the block in under five minutes"]
    $ aroundTheBlock
  where
    aroundTheBlock =
      gate (locWithin p1 tolerance)
        . gate (locWithin p2 tolerance)
        . andThen (points 3)
        . gate (locWithin p3 tolerance)
        . andThen (points 4)
        . gate (locWithin p4 tolerance)
        . andThen (points 5)
        . gate (locWithin p1 tolerance)
        $ points 10
    points = reward . Points
    fiveMinutes = Time 300
    p1 = Point 0 0
    p2 = Point 0 40
    p3 = Point 40 40
    p4 = Point 40 0
    tolerance = Distance 10

-- Gate
--   (And (Custom IsLocation)
--        (Custom (Within (Point 0.0 0.0) (Distance 10.0))))
--   (Gate
--     (And (Custom IsLocation)
--          (Custom (Within (Point 0.0 40.0) (Distance 10.0))))
--     (RewardThen
--       (Points 3)
--       (Gate
--         (And (Custom IsLocation)
--              (Custom (Within (Point 40.0 40.0) (Distance 10.0))))
--         (RewardThen
--           (Points 4)
--           (Gate
--             (And (Custom IsLocation)
--                  (Custom (Within (Point 40.0 0.0) (Distance 10.0))))
--             (RewardThen
--               (Points 5)
--               (Gate
--                 (And (Custom IsLocation)
--                      (Custom (Within (Point 0.0 0.0) (Distance 10.0))))
--                 (RewardThen
--                   (Points 10)
--                   Empty))))))))

successfulRun :: [Input]
successfulRun =
  [ t 1,
    loc 0 0,
    t 60,
    loc 0 40,
    t 120,
    loc 42 38,
    t 180,
    loc 37 1,
    t 240,
    t 300,
    loc 1 2,
    t 301,
    t 302
  ]
  where
    t n = EventTime (Time n)
    loc x y = EventLocation (Point x y)

-- >>> runGame successfulRun
-- (Results {rewards = Points 22, clues = MonoidalMap {getMonoidalMap = fromList [([Hint "Run around the block in under five minutes"],Completed)]}},Empty)

failedRun :: [Input]
failedRun =
  [ t 1,
    loc 0 0,
    t 60,
    loc 0 40,
    t 120,
    loc 42 38,
    t 180,
    loc 37 1,
    t 240,
    t 300,
    t 301,
    loc 1 2,
    t 302
  ]
  where
    t n = EventTime (Time n)
    loc x y = EventLocation (Point x y)

-- >>> runGame failedRun
-- (Results {rewards = Points 12, clues = MonoidalMap {getMonoidalMap = fromList [([Hint "Run around the block in under five minutes"],Failed)]}},Empty)

-- >>> let c = rewardThen 10 empty :: Challenge Input Hint Rewards in pumpChallenge c []
-- (Results {rewards = Points 10, clues = MonoidalMap {getMonoidalMap = fromList []}},Empty)

-- >>> let c = rewardThen 10 empty :: Challenge Input Hint Rewards in pumpChallenge (eitherC c bottom) []
-- (Results {rewards = Points 10, clues = MonoidalMap {getMonoidalMap = fromList []}},Gate Never Empty)

timeout :: Time -> Challenge Input k Rewards -> Challenge Input k Rewards
timeout t c = eitherC (gate (afterTime t) empty) c

pointOfInterest :: [k] -> Point -> Distance -> Rewards -> Challenge Input k Rewards
pointOfInterest k p d r = clue k (gate (photoWithin p d) (reward r))

-- Filters
locWithin :: Point -> Distance -> InputFilter Input
locWithin target distance = custom IsLocation `andF` custom (Within target distance)

photoWithin :: Point -> Distance -> InputFilter Input
photoWithin target distance = custom IsPhoto `andF` custom (Within target distance)

squaredDistance :: Point -> Point -> Double
squaredDistance (Point ax ay) (Point bx by) =
  let dx = ax - bx
      dy = ay - by
   in dx * dx + dy * dy

photoAbove :: Altitude -> InputFilter Input
photoAbove threshold = custom IsPhoto `andF` custom (Above threshold)

afterTime :: Time -> InputFilter Input
afterTime t0 = custom $ AfterTime t0

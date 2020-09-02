module ADD.Scavenger where

data Challenge
data Input
data Reward
data Clue
data Point
data Distance
data Altitude
data Photo

getRewards :: Challenge -> [Input] -> [Reward]
getRewards _ _ = undefined

pointOfInterest :: Clue -> Point -> Distance -> Reward -> Challenge
pointOfInterest c p d r = clue c (photoWithin p d (reward r))

photo :: Point -> Photo -> Input
photo _ _ = undefined

isPhoto :: Input -> Bool
isPhoto _ = undefined

within :: Point -> Point -> Distance -> Bool
within _ _ _ = undefined

aboveAltitude :: Point -> Altitude -> Bool
aboveAltitude _ _ = undefined

clue :: Clue -> Challenge -> Challenge
clue _ _ = undefined

-- Law "getRewards/clue"
-- forall k c is.
--   getRewards (clue k c) is
--   = getRewards c is

reward :: Reward -> Challenge
reward _ = undefined

-- Law "getRewards/reward"
-- forall r is.
--   getRewards (reward r) is = [r]

photoWithin :: Point -> Distance -> Challenge -> Challenge
photoWithin _ _ _ = undefined

-- Law "getRewards/photoWithin"
-- forall poi p pic d c is.
--   within poi p d =>
--     getRewards
--       (photoWithin poi d c)
--       (photo p pic : is)
--     = getRewards c is
-- Law "getRewards/photoWithin not within"
-- forall poi p pic d c is.
--   not(within poi p d) =>
--     getRewards (photoWithin poi d c) (photo p pic : is)
--     = getRewards (photoWithin poi d c) is
-- Law "getRewards/photoWithin not photo"
-- forall poi d c i is.
--   not(isPhoto i) =>
--     getRewards (photoWithin poi d c) (i : is)
--     = getRewards (photoWithin poi d c) is

-- Law "pointOfInterest"
-- forall c p d r.
--   pointOfInterest c p d r = clue c (photoWithin p d (reward r))

photoAbove :: Altitude -> Challenge -> Challenge
photoAbove _ _ = undefined
-- Law "getRewards/photoAbove"
-- forall poi p pic d c is.
--   aboveAltitude p a =>
--     getRewards
--       (photoAbove a c)
--       (photo p pic : is)
--     = getRewards c is

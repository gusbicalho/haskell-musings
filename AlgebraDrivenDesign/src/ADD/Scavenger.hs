module ADD.Scavenger where

data Challenge
data Input
data InputFilter
data Reward
data Clue
data Point
data Distance
data Altitude
data Photo

getRewards :: Challenge -> [Input] -> [Reward]
getRewards _ _ = undefined

matches :: InputFilter -> Input -> Bool
matches _ _ = undefined

gate :: InputFilter -> Challenge -> Challenge
gate _ _ = undefined

-- Law "getRewards/gate"
-- forall f c i is.
--   matches f i =>
--     getRewards (gate f c) (i : is) = getRewards c is
-- Law "getRewards/gate unmatched"
-- forall f c i is.
--   not(matches f i) =>
--     getRewards (gate f c) (i : is) = getRewards (gate f c) is
-- Law "getRewards/gate empty"
-- forall f c.
--   getRewards (gate f c) [] = []

pointOfInterest :: Clue -> Point -> Distance -> Reward -> Challenge
pointOfInterest c p d r = clue c (gate (photoWithin p d) (reward r))

-- Law "pointOfInterest"
-- forall c p d r.
--   pointOfInterest c p d r = clue c (gate (photoWithin p d) (reward r))

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

-- InputFilters

photoWithin :: Point -> Distance -> InputFilter
photoWithin _ _ = undefined

-- Law "matches/photoWithin"
-- forall poi d p pic.
--   matches (photoWithin poi d) (photo p pic)
--   = within poi p d
-- Law "matches/photoWithin not photo"
-- forall poi d i.
--   not(isPhoto i) =>
--     matches (photoWithin poi d) i
--     = False

photoAbove :: Altitude -> InputFilter
photoAbove _ = undefined
-- Law "matches/photoAbove"
-- forall a p pic.
--   matches (photoAbove a) (photo p pic)
--   = aboveAltitude p a
-- Law "matches/photoAbove not photo"
-- forall a i.
--   not(isPhoto i) =>
--     matches (photoAbove a) i
--     = False

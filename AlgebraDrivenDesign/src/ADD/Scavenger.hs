module ADD.Scavenger where

data Challenge
data Input
data Reward

getRewards :: Challenge -> [Input] -> [Reward]
getRewards _ _ = undefined

data Clue
data Point
data Distance
data Photo

pointOfInterest :: Clue -> Point -> Distance -> Reward -> Challenge
pointOfInterest _ _ _ _ = undefined

photo :: Point -> Photo -> Input
photo _ _ = undefined

isPhoto :: Input -> Bool
isPhoto _ = undefined

within :: Point -> Point -> Distance -> Bool
within _ _ _ = undefined

-- Law "point of interest"
-- forall c poi d r p pic is.
--   within poi p d =>
--     getRewards
--       (pointOfInterest c poi d r)
--       (photo p pic : is)
--     = [r]

-- Law "outside point of interest"
-- forall c poi d r p pic is.
--   not (within poi p d) =>
--     getRewards
--       (pointOfInterest c poi d r)
--       (photo p pic : is)
--     = getRewards
--         (pointOfInterest c poi d r)
--         is

-- Law "unmatching point of interest"
-- forall c poi d r p pic is.
--   not (isPhoto i) =>
--     getRewards
--       (pointOfInterest c poi d r)
--       (i : is)
--     = getRewards
--         (pointOfInterest c poi d r)
--         is

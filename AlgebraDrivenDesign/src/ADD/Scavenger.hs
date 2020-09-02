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

within :: Point -> Point -> Distance -> Bool
within _ _ _ = undefined

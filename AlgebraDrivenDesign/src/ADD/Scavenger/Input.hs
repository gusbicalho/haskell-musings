module ADD.Scavenger.Input where

import ADD.Scavenger.Types

time :: Time -> Input
time _ = undefined

photo :: Point -> Photo -> Input
photo _ _ = undefined

isPhoto :: Input -> Bool
isPhoto _ = undefined

location :: Point -> Input
location _ = undefined

isLocation :: Input -> Bool
isLocation _ = undefined

within :: Point -> Point -> Distance -> Bool
within _ _ _ = undefined

aboveAltitude :: Point -> Altitude -> Bool
aboveAltitude _ _ = undefined

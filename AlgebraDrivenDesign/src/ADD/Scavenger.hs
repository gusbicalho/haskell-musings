module ADD.Scavenger where

import ADD.Scavenger.Types
import ADD.Scavenger.InputFilter

getRewards :: Challenge -> [Input] -> [Reward]
getRewards _ _ = undefined

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

pointOfInterest :: Clue -> Point -> Distance -> Reward -> Challenge
pointOfInterest c p d r = clue c (gate (photoWithin p d) (reward r))

-- Law "pointOfInterest"
-- forall c p d r.
--   pointOfInterest c p d r = clue c (gate (photoWithin p d) (reward r))

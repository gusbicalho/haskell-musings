module ADD.Scavenger where

import ADD.Scavenger.InputFilter
import ADD.Scavenger.Types
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet

getRewards :: Challenge -> [Input] -> MultiSet Reward
getRewards _ _ = undefined

completes :: Challenge -> [Input] -> Maybe [Input]
completes _ _ = undefined

-- Law "completes/empty"
-- forall is.
--   completes empty is = Just is
-- Law "completes/reward"
-- forall r is.
--   completes (reward r) is = Just is
-- Law "completes/both"
-- forall c1 c2 is.
--   completes (both c1 c2) is = shorterOf <$> completes c1 is <*> completes c2 is
-- Law "completes/clue"
-- forall r is.
--   completes (clue k c) is = completes c is
-- Law "completes/gate"
-- forall f c i is.
--   matches f i =>
--     completes (gate f c) (i : is) = completes c is
-- Law "completes/gate unmatched"
-- forall f c i is.
--   not(matches f i) =>
--     completes (gate f c) (i : is) = completes (gate f c) is
-- Law "completes/gate empty"
-- forall f c.
--   completes (gate f c) [] = []
-- Law "completes/andThen"
-- forall c1 c2 is.
--   completes (andThen c1 c2) is = completes c1 is >>= completes c2

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
--   getRewards (gate f c) [] = mempty

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
--   getRewards (reward r) is = MultiSet.singleton r

empty :: Challenge
empty = undefined

-- Law "getRewards/empty"
-- forall is.
--   getRewards empty is = []

andThen :: Challenge -> Challenge -> Challenge
andThen _ _ = undefined

-- Law "getRewards/andThen"
-- forall c1 c2 is is'.
--   completes c1 is = Just is' =>
--     getRewards (andThen c1 c2) is
--     = getRewards c1 is <> getRewards c2 js
-- Law "getRewards/andThen incomplete"
-- forall c1 c2 is.
--   completes c1 is = Nothing =>
--     getRewards (andThen c1 c2) is
--     = getRewards c1 is
-- Law "andThen/identity"
-- forall c.
--   andThen c empty = c = andThen empty c
-- Law "andThen/associative"
-- forall c1 c2 c3.
--   andThen c1 (andThen c2 c3) = andThen (andThen c1 c2) c3
-- Law "andThen/gate"
-- forall f c1 c2.
--   andThen (gate f c1) c2 = gate f (andThen c1 c2)

pointOfInterest :: Clue -> Point -> Distance -> Reward -> Challenge
pointOfInterest c p d r = clue c (gate (photoWithin p d) (reward r))

-- Law "pointOfInterest"
-- forall c p d r.
--   pointOfInterest c p d r = clue c (gate (photoWithin p d) (reward r))

both :: Challenge -> Challenge -> Challenge
both _ _ = undefined

-- Law "both:identity"
-- forall c .
--   both empty c = c = both c empty
-- Law "both:commutative"
-- forall c1 c2.
--   both c1 c2 = both c2 c1
-- Law "both:associative"
-- forall c1 c2 c3.
--   both c1 (both c2 c3) = both (both c1 c2) c3
-- Law "getRewards/both"
-- forall c1 c2 is.
--   getRewards (both c1 c2) is = getRewards c1 is <> getRewards c2 is
-- Law "both/andThen/reward"
-- forall r c1 c2.
--   both (andThen (reward r) c1) c2
--   = andThen (reward r) (both c1 c2)

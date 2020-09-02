module ADD.Scavenger where

import ADD.Scavenger.InputFilter
import ADD.Scavenger.Types
import Control.Monad (foldM)
import Data.MultiSet (MultiSet)

isEmpty :: Challenge -> Bool
isEmpty _ = undefined

isReward :: Challenge -> Bool
isReward _ = undefined

pumpChallenge :: Challenge -> [Input] -> (MultiSet Reward, Challenge)
pumpChallenge c = foldM (flip step) c . (Nothing :) . fmap Just

getRewards :: Challenge -> [Input] -> MultiSet Reward
getRewards c is = fst $ pumpChallenge c is

completes :: Challenge -> [Input] -> Bool
completes c is = isEmpty . snd $ pumpChallenge c is

step :: Maybe Input -> Challenge -> (MultiSet Reward, Challenge)
step _ _ = (undefined, undefined)

-- Law "step/empty"
-- forall i.
--   step i empty = pure empty
-- Law "step/reward"
-- forall i r.
--   step i (reward r) = (MultiSet.singleton r, empty)
-- Law "step/both"
-- forall i c1 c2.
--   step i (both c1 c2) = both <$> step i c1 <*> step i c2
-- Law "step/eitherC"
-- forall i c1 c2.
--   step i (eitherC c1 c2) = eitherC <$> step i c1 <*> step i c2
-- Law "step/clue"
-- forall i k c.
--   step i (clue k c) = step i c
-- Law "step/gate"
-- forall i f c.
--   matches f i =>
--     step (Just i) (gate f c) = step Nothing c
-- Law "step/gate unmatched"
-- forall i f c.
--   not(matches f i) =>
--     step (Just i) (gate f c) = pure (gate f c)
-- Law "step/gate nothing"
-- forall f c.
--   step Nothing (gate f c) = pure (gate f c)
-- Law "step/andThen"
-- forall i c1 c2 r.
--   step i c1 == (r, empty) =>
--     step i (andThen c1 c2) = join (r, step Nothing c2)
-- Law "step/andThen incomplete"
-- forall i c1 c2.
--   step i c1 == (_, c1') && not (isEmpty c1') =>
--     step i (andThen c1 c2) = andThen <$> (step i c1) <*> pure c2

gate :: InputFilter -> Challenge -> Challenge
gate _ _ = undefined

clue :: Clue -> Challenge -> Challenge
clue _ _ = undefined

reward :: Reward -> Challenge
reward _ = undefined

empty :: Challenge
empty = undefined

andThen :: Challenge -> Challenge -> Challenge
andThen _ _ = undefined

-- Law "andThen/identity"
-- forall c.
--   andThen c empty = c = andThen empty c
-- Law "andThen/associative"
-- forall c1 c2 c3.
--   andThen c1 (andThen c2 c3) = andThen (andThen c1 c2) c3
-- Law "andThen/gate"
-- forall f c1 c2.
--   andThen (gate f c1) c2 = gate f (andThen c1 c2)

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
-- Law "both/andThen/reward"
-- forall r c1 c2.
--   both (andThen (reward r) c1) c2
--   = andThen (reward r) (both c1 c2)

eitherC :: Challenge -> Challenge -> Challenge
eitherC _ _ = undefined

-- Law "eitherC:identity"
-- forall c.
--   eitherC bottom c = c = eitherC c bottom
-- Law "eitherC:commutative"
-- forall c1 c2.
--   eitherC c1 c2 = eitherC c2 c1
-- Law "eitherC:associative"
-- forall c1 c2 c3.
--   eitherC c1 (eitherC c2 c3) = eitherC (eitherC c1 c2) c3
-- Law "eitherC/andThen/reward"
-- forall r c1 c2.
--   either (andThen (reward r) c1) c2
--   = andThen (reward r) (either c1 c2)
-- Law "eitherC/empty"
-- forall c.
--   not (isReward c) =>
--     either empty c = empty

bottom :: Challenge
bottom = undefined
-- Law "bottom"
-- forall c.
--   bottom = gate never c

timeout :: Time -> Challenge -> Challenge
timeout _ _ = undefined
-- Law "timeout"
-- forall t c.
--   timeout t c = eitherC (gate (afterTime t) empty) c

pointOfInterest :: Clue -> Point -> Distance -> Reward -> Challenge
pointOfInterest c p d r = clue c (gate (photoWithin p d) (reward r))

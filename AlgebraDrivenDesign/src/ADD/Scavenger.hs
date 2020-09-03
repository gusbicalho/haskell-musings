{-# LANGUAGE ConstraintKinds #-}
module ADD.Scavenger where

import ADD.Scavenger.Clue
import ADD.Scavenger.InputFilter
import ADD.Scavenger.Types
import Control.Monad (foldM)
import Data.Map.Monoidal.Strict (MonoidalMap)

isEmpty :: Challenge i r -> Bool
isEmpty _ = undefined

isReward :: Challenge i r -> Bool
isReward _ = undefined

findClues :: Clue -> Challenge i r -> MonoidalMap Clue ClueState
findClues _ _ = undefined

class Commutative r where

type ValidReward r = (Monoid r, Commutative r)
type ChallengeOutput r = (MonoidalMap Clue ClueState, r)

pumpChallenge :: ValidReward r => Challenge i r -> [Input] -> (ChallengeOutput r, Challenge i r)
pumpChallenge c = foldM (flip $ step noClue) c . (Nothing :) . fmap Just

getRewards :: ValidReward r => Challenge i r -> [Input] -> r
getRewards c is = snd . fst $ pumpChallenge c is

completes :: ValidReward r => Challenge i r -> [Input] -> Bool
completes c is = isEmpty . snd $ pumpChallenge c is

step :: ValidReward r => Clue -> Maybe Input -> Challenge i r -> (ChallengeOutput r, Challenge i r)
step _ _ _ = (undefined, undefined)

-- Law "step/empty"
-- forall kctx i.
--   step kctx i empty = pure empty
-- Law "step/reward"
-- forall kctx i r.
--   step kctx i (reward r) = ((mempty, MultiSet.singleton r), empty)
-- Law "step/both"
-- forall kctx i c1 c2.
--   step kctx i (both c1 c2) = both <$> step kctx i c1 <*> step kctx i c2
-- Law "step/eitherC empty"
-- forall kctx i c1 c2 c2' z1 z2.
--   step kctx i c1 == (z1, empty) && step kctx i c2 == (z2, c2') =>
--     step kctx i (eitherC c1 c2)
--     = fmap seenToFailed (findClues kctx c2')
--         *> step kctx i c2
--         *> step kctx i c1
-- Law "step/eitherC non empty"
-- forall kctx i c1 c2 z1 c1' z2 c2'.
--   step kctx i c1 == (z1, c1') &&
--   step kctx i c2 == (z2, c2') &&
--   not(isEmpty c1) && not(isEmpty c2) =>
--     step kctx i (eitherC c1 c2) = eitherC <$> step kctx i c1 <*> step kctx i c2
-- Law "step/clue/empty"
-- forall kctx i k.
--   step kctx i (clue k empty) = ((MonoidalMap.singleton (sub kctx k) completed, mempty), empty)
-- Law "step/clue/non empty"
-- forall kctx i k c.
--   not(isEmpty c) =>
--     step kctx i (clue k c)
--     = tell (MonoidalMap.singleton (sub kctx k) seen, mempty)
--         *> clue k <$> step (sub kctx k) i c )
-- Law "step/gate"
-- forall kctx i f c.
--   matches f i =>
--     step kctx (Just i) (gate f c) = step kctx Nothing c
-- Law "step/gate unmatched"
-- forall kctx i f c.
--   not(matches f i) =>
--     step kctx (Just i) (gate f c) = pure (gate f c)
-- Law "step/gate nothing"
-- forall f c.
--   step kctx Nothing (gate f c) = pure (gate f c)
-- Law "step/andThen"
-- forall kctx i c1 c2 r.
--   step kctx i c1 == (r, empty) =>
--     step kctx i (andThen c1 c2) = join (r, step kctx Nothing c2)
-- Law "step/andThen incomplete"
-- forall kctx i c1 c2.
--   step kctx i c1 == (_, c1') && not (isEmpty c1') =>
--     step kctx i (andThen c1 c2) = andThen <$> (step kctx i c1) <*> pure c2

gate :: InputFilter i -> Challenge i r -> Challenge i r
gate _ _ = undefined

clue :: Clue -> Challenge i r -> Challenge i r
clue _ _ = undefined

-- Law "clue/noClue"
-- forall c.
--   clue noClue c = c
-- Law "clue/sub"
-- forall c k1 k2.
--   clue (sub k1 k2) c = clue k1 (clue k2 c)

reward :: r -> Challenge i r
reward _ = undefined

-- Law "reward/mempty"
-- reward mempty = empty
-- Law "reward/mappend"
-- forall r1 r2.
--   reward (r1 <> r2) = andThen (reward r1) (reward r2)

empty :: Challenge i r
empty = undefined

andThen :: Challenge i r -> Challenge i r -> Challenge i r
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

both :: Challenge i r -> Challenge i r -> Challenge i r
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

eitherC :: Challenge i r -> Challenge i r -> Challenge i r
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

bottom :: Challenge i r
bottom = undefined

-- Law "bottom"
-- forall c.
--   bottom = gate never c

timeout :: Time -> Challenge i r -> Challenge i r
timeout _ _ = undefined

-- Law "timeout"
-- forall t c.
--   timeout t c = eitherC (gate (afterTime t) empty) c

pointOfInterest :: Clue -> Point -> Distance -> r -> Challenge Input r
pointOfInterest c p d r = clue c (gate (photoWithin p d) (reward r))

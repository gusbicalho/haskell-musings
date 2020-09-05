{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ADD.Scavenger.Algebra.Challenge where

import ADD.Scavenger.Algebra.ClueState (ClueState, completed, failed, seen)
import ADD.Scavenger.Algebra.InputFilter (CustomFilter, HasFilter, InputFilter, matches, never)
import Control.Monad (foldM)
import Data.Map.Monoidal.Strict (MonoidalMap)
import qualified Data.Map.Monoidal.Strict as MonoidalMap
import GHC.Generics (Generic)
import Generic.Data (Generically (Generically))

data Challenge i k r
  = Empty
  | Gate (InputFilter i) (Challenge i k r)
  | Clue k (Challenge i k r)
  | RewardThen r (Challenge i k r)
  | EitherC (Challenge i k r) (Challenge i k r)
  | Both (Challenge i k r) (Challenge i k r)
  | AndThen (Challenge i k r) (Challenge i k r)
  deriving stock (Generic)
deriving instance (Eq k, Eq r, Eq (CustomFilter i)) => Eq (Challenge i k r)
deriving instance (Ord k, Ord r, Ord (CustomFilter i)) => Ord (Challenge i k r)
deriving instance (Show k, Show r, Show (CustomFilter i)) => Show (Challenge i k r)

isEmpty :: Challenge i k r -> Bool
isEmpty Empty = True
isEmpty _ = False

isReward :: Challenge i k r -> Bool
isReward RewardThen {} = True
isReward _ = False

findClues :: Ord k => [k] -> Challenge i k r -> MonoidalMap [k] ClueState
findClues _ Empty = mempty
findClues kctx (Both c1 c2) = findClues kctx c1 <> findClues kctx c2
findClues kctx (EitherC c1 c2) = findClues kctx c1 <> findClues kctx c2
findClues _ (Gate {}) = mempty
findClues kctx (AndThen c _) = findClues kctx c
findClues kctx (RewardThen _ c) = findClues kctx c
findClues kctx (Clue k Empty) = MonoidalMap.singleton (kctx <> [k]) completed
findClues kctx (Clue k c) =
  let kctx' = kctx <> [k]
   in MonoidalMap.singleton kctx' seen <> findClues kctx' c

class Semigroup r => Commutative r

type ValidInput i = HasFilter i

type ValidReward r = (Eq r, Monoid r, Commutative r)

type ValidClue k = Ord k

type ValidChallenge i k r = (ValidInput i, ValidClue k, ValidReward r)

data Results k r = Results
  { rewards :: r,
    clues :: MonoidalMap [k] ClueState
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Semigroup, Monoid) via Generically (Results k r)

pumpChallenge :: ValidChallenge i k r => Challenge i k r -> [i] -> (Results k r, Challenge i k r)
pumpChallenge c = foldM (flip $ step mempty) c . (Nothing :) . fmap Just

runChallenge :: (HasFilter i, Ord k, Monoid r, Commutative r, Eq r) => Challenge i k r -> [i] -> (Results k r, Bool)
runChallenge c = fmap isEmpty . pumpChallenge c

getRewards :: ValidChallenge i k r => Challenge i k r -> [i] -> r
getRewards c is = rewards . fst $ pumpChallenge c is

completes :: ValidChallenge i k r => Challenge i k r -> [i] -> Bool
completes c is = isEmpty . snd $ pumpChallenge c is

getClues :: ValidChallenge i k r => Challenge i k r -> [i] -> MonoidalMap [k] ClueState
getClues c is = clues . fst $ pumpChallenge c is

step :: ValidChallenge i k r => [k] -> Maybe i -> Challenge i k r -> (Results k r, Challenge i k r)
step _ _ Empty = pure empty
step kctx i (Both c1 c2) = both <$> step kctx i c1 <*> step kctx i c2
step kctx i (RewardThen r c) = tellReward r *> step kctx i c
step kctx (Just i) (Gate f c)
  | matches f i = step kctx Nothing c
step _ _ c@(Gate {}) = pure c
step kctx i (AndThen c1 c2) =
  step kctx i c1 >>= \case
    Empty -> step kctx Nothing c2
    c1' -> pure $ andThen c1' c2
step kctx i (EitherC c1 c2) = do
  c1' <- step kctx i c1
  c2' <- step kctx i c2
  case (c1', c2') of
    (Empty, _) -> pruneClues kctx c2'
    (_, Empty) -> pruneClues kctx c1'
    _ -> pure $ eitherC c1' c2'
step kctx i (Clue k c) = do
  step kctx' i c >>= \case
    Empty -> do
      tellClue kctx' completed
      pure empty
    c' -> do
      tellClue kctx' seen
      pure $ clue [k] c'
  where
    kctx' = kctx <> [k]

pruneClues :: (Monoid r, Ord k) => [k] -> Challenge i k r -> (Results k r, Challenge i k r)
pruneClues kctx c = do
  tellClues $ fmap (<> failed) $ findClues kctx c
  pure empty

tellClue :: Monoid r => [k] -> ClueState -> (Results k r, ())
tellClue ks state = tellClues (MonoidalMap.singleton ks state)

tellClues :: Monoid r => MonoidalMap [k] ClueState -> (Results k r, ())
tellClues ks = (Results mempty ks, ())

tellReward :: Ord k => r -> (Results k r, ())
tellReward r = (Results r mempty, ())

-- emptyResults :: forall k r. (ValidClue k, ValidReward r) => Results k r
-- emptyResults = Results mempty mempty

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
--     = pruneClues (findClues kctx c2')
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

empty :: Challenge i k r
empty = Empty

rewardThen :: (Eq r, Monoid r) => r -> Challenge i k r -> Challenge i k r
rewardThen r (RewardThen r' c) = rewardThen (r <> r') c
rewardThen r c
  | r == mempty = c
  | otherwise = RewardThen r c

reward :: (Eq r, Monoid r) => r -> Challenge i k r
reward r = rewardThen r empty

-- Law "reward/mempty"
-- reward mempty = empty
-- Law "reward/mappend"
-- forall r1 r2.
--   reward (r1 <> r2) = andThen (reward r1) (reward r2)
-- Law "RewardThen"
-- forall r c.
--   RewardThen r c = andThen (reward r) c

gate :: InputFilter i -> Challenge i k r -> Challenge i k r
gate f c = Gate f c

andThen :: (Eq r, Monoid r) => Challenge i k r -> Challenge i k r -> Challenge i k r
andThen c1 Empty = c1
andThen Empty c2 = c2
andThen (RewardThen r c1) c2 = rewardThen r (andThen c1 c2)
andThen (Gate f c1) c2 = gate f (andThen c1 c2)
andThen (AndThen c1 c2) c3 = andThen c1 (andThen c2 c3)
andThen c1 c2 = AndThen c1 c2

-- Law "andThen/identity"
-- forall c.
--   andThen c empty = c = andThen empty c
-- Law "andThen/associative"
-- forall c1 c2 c3.
--   andThen c1 (andThen c2 c3) = andThen (andThen c1 c2) c3
-- Law "andThen/gate"
-- forall f c1 c2.
--   andThen (gate f c1) c2 = gate f (andThen c1 c2)

both :: (Eq r, Monoid r) => Challenge i k r -> Challenge i k r -> Challenge i k r
both c1 Empty = c1
both Empty c2 = c2
both (RewardThen r c1) c2 = rewardThen r (both c1 c2)
both c1 (RewardThen r c2) = rewardThen r (both c1 c2)
both c1 c2 = Both c1 c2

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

eitherC :: (Eq r, Monoid r) => Challenge i k r -> Challenge i k r -> Challenge i k r
eitherC (RewardThen r c1) c2 = rewardThen r (both c1 c2)
eitherC c1 (RewardThen r c2) = rewardThen r (both c1 c2)
eitherC c1 c2 = EitherC c1 c2

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

bottom :: Challenge i k r
bottom = gate never empty

-- Law "bottom"
-- forall c.
--   bottom = gate never c

clue :: (Eq r, Monoid r) => [k] -> Challenge i k r -> Challenge i k r
clue [] c = c
clue k (RewardThen r c) = rewardThen r (clue k c)
clue k c = foldr Clue c k

-- Law "clue/mempty"
-- forall c.
--   clue mempty c = c
-- Law "clue/mappend"
-- forall c k1 k2.
--   clue (k1 <> k2) c = clue k1 (clue k2 c)

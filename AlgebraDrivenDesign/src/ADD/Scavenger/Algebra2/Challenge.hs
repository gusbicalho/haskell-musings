{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module ADD.Scavenger.Algebra2.Challenge where

import ADD.Scavenger.Algebra2.ClueState (ClueState, completed, failed, seen)
import ADD.Scavenger.Algebra2.InputFilter (CustomFilter, HasFilter, InputFilter, matches)
import Control.Monad.ST (ST, runST)
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Foldable (Foldable (fold))
import Data.Map.Monoidal.Strict (MonoidalMap)
import qualified Data.Map.Monoidal.Strict as MonoidalMap
import Data.Monoid (Any (Any), Endo (Endo), getAny)
import Data.STRef.Strict (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import Data.Traversable (for)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Generic.Data (Generically (Generically))

data ChallengeData i k r s = ChallengeData
  { waitingOn :: !(MonoidalMap (InputFilter i) (ST s (ChallengeData i k r s))),
    results :: !(Results k r),
    isComplete' :: !Any
  }
  deriving (Generic)

deriving via
  Generically (ChallengeData i k r s)
  instance
    (Semigroup r, Ord k, Ord (CustomFilter i)) =>
    Semigroup (ChallengeData i k r s)

deriving via
  Generically (ChallengeData i k r s)
  instance
    (Monoid r, Ord k, Ord (CustomFilter i)) =>
    Monoid (ChallengeData i k r s)

isComplete :: ChallengeData i k r s -> Bool
isComplete = getAny . isComplete'

data ClueContext k s = ClueContext
  { currentClue :: DList k,
    recordClueState :: DList k -> ClueState -> ST s ClueState
  }

noClueContext :: ClueContext k s
noClueContext = ClueContext mempty (const pure)

data Challenge i k r = Challenge
  { showChallenge :: String,
    unChallenge ::
      forall s.
      ClueContext k s ->
      ST s (ChallengeData i k r s) ->
      ST s (ChallengeData i k r s)
  }

instance Show (Challenge i k r) where
  show = showChallenge

instance
  (Semigroup r, Ord k, Ord (CustomFilter i)) =>
  Semigroup (Challenge i k r)
  where
  c1 <> c2 =
    Challenge
      { unChallenge = \cont -> do
          d1 <- unChallenge c1 cont
          d2 <- unChallenge c2 cont
          pure $ d1 <> d2,
        showChallenge = labeled2 "(<>)" c1 c2
      }

instance
  (Monoid r, Ord k, Ord (CustomFilter i)) =>
  Monoid (Challenge i k r)
  where
  mempty = bottom

class Semigroup r => Commutative r

type ValidInput i = HasFilter i

type ValidReward r = (Eq r, Monoid r, Commutative r)

type ValidClue k = Ord k

type ValidChallenge i k r =
  ( ValidInput i,
    ValidClue k,
    ValidReward r,
    Ord (CustomFilter i),
    Show i,
    Show (CustomFilter i),
    Show k,
    Show r
  )

data Results k r = Results
  { rewards :: r,
    clues :: MonoidalMap [k] ClueState
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Semigroup, Monoid) via Generically (Results k r)

pumpChallenge :: (ValidChallenge i k r) => [i] -> ChallengeData i k r s -> ST s (ChallengeData i k r s)
pumpChallenge [] cData = pure cData
pumpChallenge _ cData
  | isComplete cData = pure cData
pumpChallenge (i : is) d = pumpChallenge is =<< step i d

runChallenge :: ValidChallenge i k r => Challenge i k r -> [i] -> (Results k r, Bool)
runChallenge c is = runST $ do
  cData <- pumpChallenge is =<< (unChallenge c noClueContext end)
  pure (results cData, isComplete cData)

getRewards :: ValidChallenge i k r => Challenge i k r -> [i] -> r
getRewards c is = rewards . fst $ runChallenge c is

getClues :: ValidChallenge i k r => Challenge i k r -> [i] -> MonoidalMap [k] ClueState
getClues c is = clues . fst $ runChallenge c is

step :: (ValidChallenge i k r) => i -> ChallengeData i k r s -> ST s (ChallengeData i k r s)
step i cData = do
  pastTheGates <- for (MonoidalMap.assocs (waitingOn cData)) $ \(f, cont) -> do
    case matches f i of
      True -> do
        d' <- cont
        -- for each gate we are waitingOn, if they match the current input
        -- we return two things
        pure
          ( -- first, a function that will remove that gate from the waitingOn map
            Endo $ MonoidalMap.delete f,
            -- second, the new ChallengeData -- whatever was waiting for us after the gate
            d'
          )
      False -> mempty
  let (Endo removeMatchedGates, newChallengeData) = fold pastTheGates
  pure $
    cData {waitingOn = removeMatchedGates (waitingOn cData)}
      <> newChallengeData

tellReward ::
  forall i k r s.
  (Monoid r, Ord k, Ord (CustomFilter i)) =>
  r ->
  ChallengeData i k r s
tellReward r = mempty {results = (mempty :: Results k r) {rewards = r}}

tellClue ::
  forall i k r s.
  (Monoid r, Ord k, Ord (CustomFilter i)) =>
  [k] ->
  ClueState ->
  ChallengeData i k r s
tellClue k state = mempty {results = (mempty :: Results k r) {clues = MonoidalMap.singleton k state}}

end :: ValidChallenge i k r => ST s (ChallengeData i k r s)
end = pure $ mempty {isComplete' = Any True}

empty :: forall i k r. Challenge i k r
empty = Challenge "empty" $ \_ cont -> cont

reward :: forall i k r. ValidChallenge i k r => r -> Challenge i k r
reward r =
  Challenge
    (labeled "reward" r)
    $ \_ -> fmap (<> tellReward r)

gate :: forall i k r. ValidChallenge i k r => InputFilter i -> Challenge i k r -> Challenge i k r
gate f c =
  Challenge
    (labeled2 "gate" f c)
    $ \kctx cont ->
      pure $
        (mempty :: ChallengeData i k r s)
          { waitingOn = MonoidalMap.singleton f (unChallenge c kctx cont)
          }

andThen :: forall i k r. Challenge i k r -> Challenge i k r -> Challenge i k r
andThen c1 c2 =
  Challenge
    (labeled2 "andThen" c1 c2)
    $ \kctx -> unChallenge c1 kctx . unChallenge c2 kctx

both :: forall i k r. (ValidChallenge i k r) => Challenge i k r -> Challenge i k r -> Challenge i k r
both c1 c2 =
  Challenge
    (labeled2 "both" c1 c2)
    $ \kctx cont -> do
      remaining <- newSTRef (2 :: Word8)
      let cont' = do
            modifySTRef' remaining $ subtract 1
            readSTRef remaining >>= \case
              0 -> cont
              _ -> pure mempty
      d1 <- unChallenge c1 kctx cont'
      d2 <- unChallenge c2 kctx cont'
      pure $ d1 <> d2

oneShot :: Monoid b => STRef s Bool -> ST s b -> ST s b
oneShot ref action = do
  readSTRef ref >>= \case
    True -> pure mempty
    False -> do
      writeSTRef ref True
      action

eitherC :: forall i k r. (ValidChallenge i k r) => Challenge i k r -> Challenge i k r -> Challenge i k r
eitherC c1 c2 =
  Challenge
    (labeled2 "eitherC" c1 c2)
    $ \kctx cont -> do
      -- Both branches will try to fill this STRef, only one will succeed,
      -- due to oneShot. oneShot replaces the ChallengeData result from the
      -- "latest" branch into mempty, so the output from that branch in lost.
      -- Notice we only lose the output for the "latest" step of the pruned
      -- branch, that step that would happen at the same time as the final step
      -- of the completed branch.
      -- Since we process c1 branch first, this prunning is biased in that
      -- direction - if c1 and c2 complete together, c2 will be pruned.
      filled <- newSTRef False
      -- We collect all clues touched by each branch in these Refs
      -- The `decorate`d ClueContext for each branch will add each clue from
      -- that branch to the respective Ref, so that the other branch can prune
      -- it them if it completes.
      -- The `decorated`d ClueContext will also ensure that clues touched after
      -- one branch completes will be set as failed.
      c1Clues <- newSTRef mempty
      c2Clues <- newSTRef mempty
      cData1 <- unChallenge c1 (decorate filled c1Clues kctx) . oneShot filled $ do
        d <- cont
        prunedClues <- pruneClues c2Clues
        pure $ d <> prunedClues
      cData2 <- unChallenge c2 (decorate filled c2Clues kctx) . oneShot filled $ do
        d <- cont
        prunedClues <- pruneClues c1Clues
        pure $ d <> prunedClues
      pure $ cData1 <> cData2
  where
    decorate :: forall s. STRef s Bool -> STRef s [(DList k)] -> ClueContext k s -> ClueContext k s
    decorate filledRef cluesRef kctx =
      kctx
        { recordClueState = \ks clueState ->
            readSTRef filledRef >>= \case
              True -> recordClueState kctx ks failed
              False -> do
                modifySTRef' cluesRef (ks :)
                recordClueState kctx ks clueState
        }
    pruneClues :: forall s. STRef s [(DList k)] -> ST s (ChallengeData i k r s)
    pruneClues clues = foldMap (\k -> tellClue (DList.toList k) failed) <$> readSTRef clues

bottom :: forall i k r. (Monoid r, Ord k, Ord (CustomFilter i)) => Challenge i k r
bottom = Challenge "bottom" (\_ -> mempty)

clue :: forall i k r. (ValidChallenge i k r) => [k] -> Challenge i k r -> Challenge i k r
clue [] c = c
clue (k : ks) c =
  Challenge
    (labeled2 "clue" (k : ks) c)
    $ \kctx cont -> do
      let clueName = currentClue kctx <> DList.singleton k
          clueName' = DList.toList $ currentClue kctx'
          kctx' = kctx {currentClue = clueName}
      -- we attempt to mark the clue as seen, but the ClueContext may decide
      -- to return a different state, in which case we will use that one
      clueState <- recordClueState kctx clueName seen
      -- we run the challenge with the remaining clues, and when/if it completes,
      -- we will set the clue to completed
      contData <- unChallenge (clue ks c) kctx' $ do
        d <- cont
        pure $ tellClue clueName' completed <> d
      pure $ tellClue clueName' clueState <> contData

labeled :: (Show a) => String -> a -> String
labeled label a = label <> " (" <> show a <> ")"

labeled2 :: (Show a, Show b) => String -> a -> b -> String
labeled2 label a b = label <> " (" <> show a <> ") (" <> show b <> ")"

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
  { unChallenge ::
      forall s.
      ClueContext k s ->
      ST s (ChallengeData i k r s) ->
      ST s (ChallengeData i k r s),
    showChallenge :: String
  }

instance (ValidChallenge i k r) => Show (Challenge i k r) where
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
        showChallenge =
          "("
            <> showChallenge c1
            <> ") <> ("
            <> showChallenge c2
            <> ")"
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
empty = Challenge (\_ cont -> cont) "empty"

reward :: forall i k r. ValidChallenge i k r => r -> Challenge i k r
reward r =
  Challenge
    ( \_ cont ->
        fmap
          (<> tellReward r)
          cont
    )
    ("reward (" <> show r <> ")")

gate :: forall i k r. ValidChallenge i k r => InputFilter i -> Challenge i k r -> Challenge i k r
gate f c =
  Challenge
    ( \kctx cont -> do
        pure $
          (mempty :: ChallengeData i k r s)
            { waitingOn = MonoidalMap.singleton f (unChallenge c kctx cont)
            }
    )
    ("gate (" <> show f <> ") (" <> show c <> ")")

andThen :: forall i k r. ValidChallenge i k r => Challenge i k r -> Challenge i k r -> Challenge i k r
andThen c1 c2 =
  Challenge
    (\kctx -> unChallenge c1 kctx . unChallenge c2 kctx)
    ("andThen (" <> show c1 <> ") (" <> show c2 <> ")")

both :: forall i k r. (ValidChallenge i k r) => Challenge i k r -> Challenge i k r -> Challenge i k r
both c1 c2 =
  Challenge
    ( \kctx cont -> do
        remaining <- newSTRef (2 :: Word8)
        let cont' = do
              modifySTRef' remaining $ subtract 1
              readSTRef remaining >>= \case
                0 -> cont
                _ -> pure mempty
        d1 <- unChallenge c1 kctx cont'
        d2 <- unChallenge c2 kctx cont'
        pure $ d1 <> d2
    )
    ("both (" <> show c1 <> ") (" <> show c2 <> ")")

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
    ( \kctx cont -> do
        filled <- newSTRef False
        c1Clues <- newSTRef mempty
        c2Clues <- newSTRef mempty
        d1 <- unChallenge c1 (decorate filled c1Clues kctx) . oneShot filled $ do
          d <- cont
          prunedClues <- pruneClues c2Clues
          pure $ d <> prunedClues
        d2 <- unChallenge c2 (decorate filled c2Clues kctx) . oneShot filled $ do
          d <- cont
          prunedClues <- pruneClues c1Clues
          pure $ d <> prunedClues
        pure $ d1 <> d2
    )
    ("eitherC (" <> show c1 <> ") (" <> show c2 <> ")")
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
bottom = Challenge (\_ -> mempty) "bottom"

clue :: forall i k r. (ValidChallenge i k r) => [k] -> Challenge i k r -> Challenge i k r
clue [] c = c
clue (k : ks) c =
  Challenge
    ( \kctx cont -> do
        let clueName = currentClue kctx <> DList.singleton k
            clueName' = DList.toList $ currentClue kctx'
            kctx' = kctx {currentClue = clueName}
        clueState <- recordClueState kctx clueName seen
        contData <- unChallenge (clue ks c) kctx' $ do
          d <- cont
          pure $ tellClue clueName' completed <> d
        pure $ tellClue clueName' clueState <> contData
    )
    ("clue (" <> show (k : ks) <> ") (" <> show c <> ")")

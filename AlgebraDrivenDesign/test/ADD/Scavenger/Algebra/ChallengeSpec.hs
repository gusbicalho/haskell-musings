{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ADD.Scavenger.Algebra.ChallengeSpec where

import ADD.Scavenger.Algebra.Challenge (Challenge, Commutative, Results, ValidChallenge, andThen, both, bottom, clue, eitherC, empty, gate, reward, runChallenge)
import ADD.Scavenger.Algebra.ClueState (ClueState)
import ADD.Scavenger.Algebra.InputFilter (CustomFilter, InputFilter)
import ADD.Scavenger.Algebra.InputFilterSpec (Test (..))
import Data.Data (Proxy (Proxy))
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import QuickSpec (A, Observe (..), Sig, VariableUse (Linear), background, con, instanceOf, liftC, mono, monoObserve, monoVars, signature, variableUse, vars, withMaxTermSize)
import Test.Hspec (Spec)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    elements,
    frequency,
    resize,
    scale,
    sized,
  )

spec :: Spec
spec = pure ()

type TestReward = MultiSet Int

type TestClue = Int

sig_small_laws_opts :: Sig
sig_small_laws_opts =
  signature
    [ variableUse Linear $
        Proxy @(Challenge Test TestClue TestReward),
      withMaxTermSize 6
    ]

sig :: Sig
sig = sig_cons <> sig_types <> sig_monoid

sig_cons :: Sig
sig_cons =
  signature
    [ con "both" $ both @Test @TestClue @TestReward,
      con "eitherC" $ eitherC @Test @TestClue @TestReward,
      con "empty" $ empty @Test @TestClue @TestReward,
      con "clue" $ clue @Test @TestClue @TestReward,
      con "andThen" $ andThen @Test @TestClue @TestReward,
      con "reward" $ reward @Test @TestClue @TestReward,
      con "gate" $ gate @Test @TestClue @TestReward,
      con "bottom" $ bottom @Test @TestClue @TestReward
    ]

sig_types :: Sig
sig_types =
  signature
    [ monoObserve @(Challenge Test TestClue TestReward),
      vars ["c"] $
        Proxy @(Challenge Test TestClue TestReward),
      monoObserve @TestReward,
      vars ["r"] $ Proxy @TestReward,
      monoObserve @(InputFilter Test),
      vars ["f"] $ Proxy @(InputFilter Test),
      monoVars @(CustomFilter Test) ["f"],
      monoVars @(TestClue) ["k"],
      monoVars @(Test) ["i"],
      instanceOf @(Monoid [TestClue]),
      instanceOf @(Semigroup [TestClue]),
      instanceOf @(Monoid TestReward),
      instanceOf @(Semigroup TestReward),
      mono @(Maybe ClueState),
      mono @(ClueState)
    ]

sig_monoid :: Sig
sig_monoid =
  background
    [ con "mempty" $ liftC @(Monoid A) $ mempty @A,
      con "<>" $ liftC @(Semigroup A) $ (<>) @A
    ]

instance
  (Arbitrary i, Ord r, ValidChallenge i k r) =>
  Observe [i] (Results k r, Bool) (Challenge i k r)
  where
  observe = flip runChallenge

instance Observe () TestReward TestReward

instance Arbitrary ClueState where
  arbitrary = elements [minBound .. maxBound]

instance Commutative TestReward

instance Arbitrary TestReward where
  arbitrary = MultiSet.fromList <$> arbitrary

instance
  ( Arbitrary (CustomFilter i),
    Arbitrary r,
    Eq r,
    Monoid r,
    Arbitrary k
  ) =>
  Arbitrary (Challenge i k r)
  where
  arbitrary = sized $ \size ->
    case size <= 1 of
      True -> pure empty
      False ->
        frequency
          [ (3, pure empty),
            (3, reward <$> arbitrary),
            (3, clue <$> resize 4 arbitrary <*> arbitrary),
            (5, andThen <$> decayArbitrary 2 <*> decayArbitrary 2),
            (5, both <$> decayArbitrary 2 <*> decayArbitrary 2),
            (5, eitherC <$> decayArbitrary 2 <*> decayArbitrary 2),
            (5, gate <$> arbitrary <*> arbitrary),
            (2, pure bottom)
          ]

decayArbitrary :: Arbitrary a => Int -> Gen a
decayArbitrary n = scale (`div` n) arbitrary

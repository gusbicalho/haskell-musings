{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ADD.Scavenger.Algebra.ChallengeSpec where

import ADD.Scavenger.Algebra.Challenge (Challenge, Commutative, Results (..), Results, ValidChallenge, andThen, both, bottom, clue, eitherC, empty, gate, reward, runChallenge)
import ADD.Scavenger.Algebra.ClueState (ClueState)
import ADD.Scavenger.Algebra.InputFilter (CustomFilter, InputFilter)
import ADD.Scavenger.Algebra.InputFilterSpec (Test (..))
import Data.Data (Proxy (Proxy))
import Data.Foldable (traverse_)
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import QuickSpec ((=~=), A, Observe (..), PrintStyle (ForQuickCheck), Sig, VariableUse (Linear), background, con, instanceOf, liftC, mono, monoObserve, monoVars, signature, variableUse, vars, withMaxTermSize, withMaxTestSize, withMaxTests, withPrintStyle)
import Test.Hspec (Spec)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    Property,
    elements,
    frequency,
    property,
    resize,
    scale,
    sized,
  )

spec :: Spec
spec =
  modifyMaxSuccess (const 100) $ traverse_ (uncurry prop) quickspec_laws

type TestReward = MultiSet Int

type TestClue = Int

sig_small_laws_opts :: Sig
sig_small_laws_opts =
  signature
    [ variableUse Linear $
        Proxy @(Challenge Test TestClue TestReward),
      withMaxTermSize 6
    ]

sig_quickCheck_opts :: Sig
sig_quickCheck_opts =
  signature
    [ withMaxTermSize 7,
      withMaxTests 1000,
      withMaxTestSize 40,
      withPrintStyle ForQuickCheck
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

quickspec_laws :: [(String, Property)]
quickspec_laws =
  [ ( "empty = reward mempty",
      property $ empty @Test @TestClue @TestReward =~= reward mempty
    ),
    ( "both c c2 = both c2 c",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward) ->
            both c c2 =~= both c2 c
    ),
    ( "eitherC c c2 = eitherC c2 c",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward) ->
            eitherC c c2 =~= eitherC c2 c
    ),
    ( "both c c = eitherC c c",
      property $
        \(c :: Challenge Test TestClue TestReward) ->
          both c c =~= eitherC c c
    ),
    ( "clue mempty c = c",
      property $
        \(c :: Challenge Test TestClue TestReward) ->
          clue mempty c =~= c
    ),
    ( "gate f bottom = bottom",
      property $
        \(f :: InputFilter Test) ->
          gate f bottom =~= bottom @Test @TestClue @TestReward
    ),
    ( "andThen c empty = c",
      property $
        \(c :: Challenge Test TestClue TestReward) ->
          andThen c empty =~= c
    ),
    ( "andThen bottom c = bottom",
      property $
        \(c :: Challenge Test TestClue TestReward) ->
          andThen bottom c =~= bottom
    ),
    ( "andThen empty c = c",
      property $
        \(c :: Challenge Test TestClue TestReward) ->
          andThen empty c =~= c
    ),
    ( "both c bottom = andThen c bottom",
      property $
        \(c :: Challenge Test TestClue TestReward) ->
          both c bottom =~= andThen c bottom
    ),
    ( "both c empty = c",
      property $
        \(c :: Challenge Test TestClue TestReward) ->
          both c empty =~= c
    ),
    ( "eitherC c bottom = c",
      property $
        \(c :: Challenge Test TestClue TestReward) ->
          eitherC c bottom =~= c
    ),
    ( "both c (reward r) = andThen (reward r) c",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (r :: MultiSet Int) ->
            both c (reward r) =~= andThen (reward r) c
    ),
    ( "eitherC empty (reward r) = reward r",
      property $
        \(r :: MultiSet Int) ->
          eitherC @Test @TestClue @TestReward empty (reward r) =~= reward r
    ),
    ( "clue (ks <> ks2) c = clue ks (clue ks2 c)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (ks :: [Int])
         (ks2 :: [Int]) ->
            clue (ks <> ks2) c =~= clue ks (clue ks2 c)
    ),
    ( "andThen (gate f c) c2 = gate f (andThen c c2)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward)
         (f :: InputFilter Test) ->
            andThen (gate f c) c2 =~= gate f (andThen c c2)
    ),
    ( "andThen (andThen c c2) c3 = andThen c (andThen c2 c3)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward)
         (c3 :: Challenge Test TestClue TestReward) ->
            andThen (andThen c c2) c3 =~= andThen c (andThen c2 c3)
    ),
    ( "both c (andThen c c2) = andThen (both c c) c2",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward) ->
            both c (andThen c c2) =~= andThen (both c c) c2
    ),
    ( "both (both c c2) c3 = both c (both c2 c3)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward)
         (c3 :: Challenge Test TestClue TestReward) ->
            both (both c c2) c3 =~= both c (both c2 c3)
    ),
    ( "eitherC c (clue ks c) = both c (clue ks c)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (ks :: [Int]) ->
            eitherC c (clue ks c) =~= both c (clue ks c)
    ),
    ( "eitherC c (both c c) = both c (both c c)",
      property $
        \(c :: Challenge Test TestClue TestReward) ->
          eitherC c (both c c) =~= both c (both c c)
    ),
    ( "eitherC (eitherC c c2) c3 = eitherC c (eitherC c2 c3)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward)
         (c3 :: Challenge Test TestClue TestReward) ->
            eitherC (eitherC c c2) c3 =~= eitherC c (eitherC c2 c3)
    ),
    ( "andThen (reward r) (reward r2) = reward (r <> r2)",
      property $
        \(r :: MultiSet Int) (r2 :: MultiSet Int) ->
          andThen @Test @TestClue @TestReward (reward r) (reward r2) =~= reward (r <> r2)
    ),
    ( "both c (clue ks empty) = andThen (clue ks empty) c",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (ks :: [Int]) ->
            both c (clue ks empty) =~= andThen (clue ks empty) c
    ),
    ( "both c (eitherC c2 empty) = andThen (eitherC c2 empty) c",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward) ->
            both c (eitherC c2 empty) =~= andThen (eitherC c2 empty) c
    ),
    ( "both c c = eitherC c (andThen c bottom)",
      property $
        \(c :: Challenge Test TestClue TestReward) ->
          both c c =~= eitherC c (andThen c bottom)
    ),
    ( "eitherC empty (gate f c) = empty",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (f :: InputFilter Test) ->
            eitherC empty (gate f c) =~= empty
    ),
    ( "eitherC empty (both c c2) = eitherC c (eitherC c2 empty)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward) ->
            eitherC empty (both c c2) =~= eitherC c (eitherC c2 empty)
    ),
    ( "andThen (reward r) (clue ks empty) = clue ks (reward r)",
      property $
        \(ks :: [Int]) (r :: MultiSet Int) ->
          andThen @Test @TestClue @TestReward (reward r) (clue ks empty) =~= clue ks (reward r)
    ),
    ( "andThen (reward r) (eitherC c empty) = eitherC c (reward r)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (r :: MultiSet Int) ->
            andThen (reward r) (eitherC c empty) =~= eitherC c (reward r)
    ),
    ( "clue ks (andThen c (reward r)) = andThen (clue ks c) (reward r)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (ks :: [Int])
         (r :: MultiSet Int) ->
            clue ks (andThen c (reward r)) =~= andThen (clue ks c) (reward r)
    ),
    ( "clue ks (andThen (reward r) c) = andThen (reward r) (clue ks c)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (ks :: [Int])
         (r :: MultiSet Int) ->
            clue ks (andThen (reward r) c) =~= andThen (reward r) (clue ks c)
    ),
    ( "both c (clue ks (reward r)) = andThen (clue ks (reward r)) c",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (ks :: [Int])
         (r :: MultiSet Int) ->
            both c (clue ks (reward r)) =~= andThen (clue ks (reward r)) c
    ),
    ( "eitherC c (andThen c (reward r)) = andThen (both c c) (reward r)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (r :: MultiSet Int) ->
            eitherC c (andThen c (reward r)) =~= andThen (both c c) (reward r)
    ),
    ( "eitherC c (andThen (reward r) c2) = andThen (reward r) (eitherC c c2)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward)
         (r :: MultiSet Int) ->
            eitherC c (andThen (reward r) c2) =~= andThen (reward r) (eitherC c c2)
    ),
    ( "both (clue ks c) (clue ks c) = clue ks (both c c)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (ks :: [Int]) ->
            both (clue ks c) (clue ks c) =~= clue ks (both c c)
    ),
    ( "both (gate f c) (gate f c2) = gate f (both c c2)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward)
         (f :: InputFilter Test) ->
            both (gate f c) (gate f c2) =~= gate f (both c c2)
    ),
    ( "both (andThen c c2) (andThen c c3) = andThen (both c c) (both c2 c3)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward)
         (c3 :: Challenge Test TestClue TestReward) ->
            both (andThen c c2) (andThen c c3) =~= andThen (both c c) (both c2 c3)
    ),
    ( "eitherC (clue ks c) (clue ks c2) = clue ks (eitherC c c2)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward)
         (ks :: [Int]) ->
            eitherC (clue ks c) (clue ks c2) =~= clue ks (eitherC c c2)
    ),
    ( "eitherC (clue ks c) (clue ks2 c) = both (clue ks c) (clue ks2 c)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (ks :: [Int])
         (ks2 :: [Int]) ->
            eitherC (clue ks c) (clue ks2 c) =~= both (clue ks c) (clue ks2 c)
    ),
    ( "eitherC (clue ks c) (both c c) = both (clue ks c) (both c c)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (ks :: [Int]) ->
            eitherC (clue ks c) (both c c) =~= both (clue ks c) (both c c)
    ),
    ( "eitherC (gate f c) (gate f c2) = gate f (eitherC c c2)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward)
         (f :: InputFilter Test) ->
            eitherC (gate f c) (gate f c2) =~= gate f (eitherC c c2)
    ),
    ( "eitherC (andThen c c2) (andThen c c3) = andThen (both c c) (eitherC c2 c3)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward)
         (c3 :: Challenge Test TestClue TestReward) ->
            eitherC (andThen c c2) (andThen c c3) =~= andThen (both c c) (eitherC c2 c3)
    ),
    ( "eitherC (both c c2) (eitherC c c2) = both (eitherC c c2) (eitherC c c2)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward) ->
            eitherC (both c c2) (eitherC c c2) =~= both (eitherC c c2) (eitherC c c2)
    ),
    ( "andThen (clue ks c) (clue ks bottom) = andThen (clue ks c) bottom",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (ks :: [Int]) ->
            andThen (clue ks c) (clue ks bottom) =~= andThen (clue ks c) bottom
    ),
    ( "andThen (clue ks c) (clue ks empty) = clue ks c",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (ks :: [Int]) ->
            andThen (clue ks c) (clue ks empty) =~= clue ks c
    ),
    ( "both (clue ks c) (clue ks bottom) = andThen (clue ks c) bottom",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (ks :: [Int]) ->
            both (clue ks c) (clue ks bottom) =~= andThen (clue ks c) bottom
    ),
    ( "eitherC (clue ks c) (andThen c bottom) = both c (clue ks c)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (ks :: [Int]) ->
            eitherC (clue ks c) (andThen c bottom) =~= both c (clue ks c)
    ),
    ( "eitherC (gate f c) (andThen c bottom) = both c (gate f c)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (f :: InputFilter Test) ->
            eitherC (gate f c) (andThen c bottom) =~= both c (gate f c)
    ),
    ( "eitherC (andThen c c2) (andThen c2 bottom) = both c2 (andThen c c2)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward) ->
            eitherC (andThen c c2) (andThen c2 bottom) =~= both c2 (andThen c c2)
    ),
    ( "eitherC (andThen c bottom) (both c c2) = both c (both c c2)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward) ->
            eitherC (andThen c bottom) (both c c2) =~= both c (both c c2)
    ),
    ( "eitherC (andThen c bottom) (both c2 c2) = eitherC c2 (both c c2)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward) ->
            eitherC (andThen c bottom) (both c2 c2) =~= eitherC c2 (both c c2)
    ),
    ( "andThen (clue ks empty) (eitherC c empty) = eitherC c (clue ks empty)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (ks :: [Int]) ->
            andThen (clue ks empty) (eitherC c empty) =~= eitherC c (clue ks empty)
    ),
    ( "andThen (eitherC c empty) (eitherC c2 empty) = eitherC c (eitherC c2 empty)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward) ->
            andThen (eitherC c empty) (eitherC c2 empty) =~= eitherC c (eitherC c2 empty)
    ),
    ( "eitherC (andThen c bottom) (andThen c2 bottom) = andThen (both c c2) bottom",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward) ->
            eitherC (andThen c bottom) (andThen c2 bottom) =~= andThen (both c c2) bottom
    ),
    ( "andThen c (both c (clue ks c)) = andThen c (clue ks (both c c))",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (ks :: [Int]) ->
            andThen c (both c (clue ks c)) =~= andThen c (clue ks (both c c))
    ),
    ( "andThen (both c (clue ks c)) c2 = both (clue ks c) (andThen c c2)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward)
         (ks :: [Int]) ->
            andThen (both c (clue ks c)) c2 =~= both (clue ks c) (andThen c c2)
    ),
    ( "andThen (both c (both c c)) c2 = both (andThen c c2) (both c c)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward) ->
            andThen (both c (both c c)) c2 =~= both (andThen c c2) (both c c)
    ),
    ( "andThen (both c (eitherC c c2)) c3 = both (andThen c c3) (eitherC c c2)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward)
         (c3 :: Challenge Test TestClue TestReward) ->
            andThen (both c (eitherC c c2)) c3 =~= both (andThen c c3) (eitherC c c2)
    ),
    ( "both c (clue ks (both c c)) = both (clue ks c) (both c c)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (ks :: [Int]) ->
            both c (clue ks (both c c)) =~= both (clue ks c) (both c c)
    ),
    ( "both c (gate f (andThen c c2)) = andThen (both c (gate f c)) c2",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward)
         (f :: InputFilter Test) ->
            both c (gate f (andThen c c2)) =~= andThen (both c (gate f c)) c2
    ),
    ( "both c (andThen c2 (andThen c c3)) = andThen (both c (andThen c2 c)) c3",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward)
         (c3 :: Challenge Test TestClue TestReward) ->
            both c (andThen c2 (andThen c c3)) =~= andThen (both c (andThen c2 c)) c3
    ),
    ( "both c (andThen (clue ks c) c2) = both (clue ks c) (andThen c c2)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward)
         (ks :: [Int]) ->
            both c (andThen (clue ks c) c2) =~= both (clue ks c) (andThen c c2)
    ),
    ( "both c (andThen (both c c2) c3) = andThen (both c (both c c2)) c3",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward)
         (c3 :: Challenge Test TestClue TestReward) ->
            both c (andThen (both c c2) c3) =~= andThen (both c (both c c2)) c3
    ),
    ( "both c (eitherC c (gate f c)) = eitherC (gate f c) (both c c)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (f :: InputFilter Test) ->
            both c (eitherC c (gate f c)) =~= eitherC (gate f c) (both c c)
    ),
    ( "both c (eitherC c (andThen c c2)) = eitherC (andThen c c2) (both c c)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward) ->
            both c (eitherC c (andThen c c2)) =~= eitherC (andThen c c2) (both c c)
    ),
    ( "both c (eitherC c (andThen c2 c)) = eitherC (andThen c2 c) (both c c)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward) ->
            both c (eitherC c (andThen c2 c)) =~= eitherC (andThen c2 c) (both c c)
    ),
    ( "both c (eitherC c (both c c2)) = eitherC (both c c) (both c c2)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward) ->
            both c (eitherC c (both c c2)) =~= eitherC (both c c) (both c c2)
    ),
    ( "eitherC c (clue ks (both c c)) = both (clue ks c) (both c c)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (ks :: [Int]) ->
            eitherC c (clue ks (both c c)) =~= both (clue ks c) (both c c)
    ),
    ( "eitherC c (andThen (clue ks c) c2) = eitherC (clue ks c) (andThen c c2)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward)
         (ks :: [Int]) ->
            eitherC c (andThen (clue ks c) c2) =~= eitherC (clue ks c) (andThen c c2)
    ),
    ( "eitherC c (both c2 (clue ks c)) = eitherC (clue ks c) (both c c2)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward)
         (ks :: [Int]) ->
            eitherC c (both c2 (clue ks c)) =~= eitherC (clue ks c) (both c c2)
    ),
    ( "eitherC c (both c (eitherC c c2)) = both (both c c) (eitherC c c2)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward) ->
            eitherC c (both c (eitherC c c2)) =~= both (both c c) (eitherC c c2)
    ),
    ( "eitherC c (both c2 (eitherC c c2)) = both (eitherC c c2) (eitherC c c2)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward) ->
            eitherC c (both c2 (eitherC c c2)) =~= both (eitherC c c2) (eitherC c c2)
    ),
    ( "andThen (clue ks (andThen c bottom)) c2 = clue ks (andThen c bottom)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward)
         (ks :: [Int]) ->
            andThen (clue ks (andThen c bottom)) c2 =~= clue ks (andThen c bottom)
    ),
    ( "both c (eitherC c (andThen c2 bottom)) = eitherC c (both c c2)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward) ->
            both c (eitherC c (andThen c2 bottom)) =~= eitherC c (both c c2)
    ),
    ( "eitherC c (andThen c2 (andThen c bottom)) = eitherC c (andThen c2 c)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward) ->
            eitherC c (andThen c2 (andThen c bottom)) =~= eitherC c (andThen c2 c)
    ),
    ( "eitherC c (andThen (clue ks empty) c2) = andThen (clue ks empty) (eitherC c c2)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward)
         (ks :: [Int]) ->
            eitherC c (andThen (clue ks empty) c2) =~= andThen (clue ks empty) (eitherC c c2)
    ),
    ( "eitherC c (andThen (eitherC c c2) bottom) = both c (eitherC c c2)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward) ->
            eitherC c (andThen (eitherC c c2) bottom) =~= both c (eitherC c c2)
    ),
    ( "eitherC c (andThen (eitherC c2 empty) c3) = andThen (eitherC c2 empty) (eitherC c c3)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward)
         (c3 :: Challenge Test TestClue TestReward) ->
            eitherC c (andThen (eitherC c2 empty) c3) =~= andThen (eitherC c2 empty) (eitherC c c3)
    ),
    ( "eitherC empty (clue ks (gate f c)) = eitherC empty (clue ks bottom)",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (f :: InputFilter Test)
         (ks :: [Int]) ->
            eitherC empty (clue ks (gate f c)) =~= eitherC empty (clue ks bottom)
    ),
    ( "eitherC empty (andThen c (gate f c2)) = eitherC c empty",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward)
         (f :: InputFilter Test) ->
            eitherC empty (andThen c (gate f c2)) =~= eitherC c empty
    ),
    ( "eitherC empty (andThen c (both c c2)) = eitherC empty (andThen c (andThen c c2))",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward) ->
            eitherC empty (andThen c (both c c2)) =~= eitherC empty (andThen c (andThen c c2))
    ),
    ( "eitherC empty (andThen c (eitherC c2 c3)) = eitherC empty (andThen c (both c2 c3))",
      property $
        \(c :: Challenge Test TestClue TestReward)
         (c2 :: Challenge Test TestClue TestReward)
         (c3 :: Challenge Test TestClue TestReward) ->
            eitherC empty (andThen c (eitherC c2 c3)) =~= eitherC empty (andThen c (both c2 c3))
    )
  ]

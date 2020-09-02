{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ADD_Old.Games.FirstModelSpec where

import ADD_Old.Games.FirstModel (runGame)
import ADD_Old.Games.FirstModel.Events (Event (..), EventFilter (..))
import ADD_Old.Games.FirstModel.Game (Game, andThen, bothG, bottom, choose, comeback, eitherG, gate, giveReward, lose, race, subgame, win)
import ADD_Old.Games.FirstModel.Result (Result (..))
import ADD_Old.Games.FirstModel.Reward (Reward (..))
import Control.Monad (replicateM)
import Data.Set (Set)
import QuickSpec
import Test.Hspec
import Test.QuickCheck hiding (Result, choose)

spec :: Spec
spec = pure ()

instance Observe [Event] (Set Reward, Maybe Result) Game where
  observe = runGame

-- Signatures

sig_types :: Sig
sig_types =
  signature
    [ monoType $ Proxy @Event,
      monoType $ Proxy @EventFilter,
      monoType $ Proxy @Reward,
      monoType $ Proxy @Result,
      monoType $ Proxy @Game,
      vars ["e"] $ Proxy @Event,
      vars ["ef"] $ Proxy @EventFilter,
      vars ["r"] $ Proxy @Reward,
      vars ["res"] $ Proxy @Result,
      vars ["g"] $ Proxy @Game
    ]

sig_results :: Sig
sig_results =
  signature
    [ con "victory" Victory,
      con "defeat" Defeat
    ]

sig_filters :: Sig
sig_filters =
  signature
    [ con "always" Always,
      con "never" Never
    ]

sig_games_core :: Sig
sig_games_core =
  signature
    [ con "win" win,
      con "lose" lose,
      con "giveReward" giveReward,
      con "andThen" andThen,
      con "subgame" subgame,
      con "eitherG" eitherG,
      con "bothG" bothG,
      con "race" race,
      con "choose" choose
    ]

sig_games_ext :: Sig
sig_games_ext =
  signature
    [ con "comeback" comeback,
      con "bottom" bottom,
      con "gate" gate
    ]

-- Arbitrary instances

instance Arbitrary Result where
  arbitrary = elements [Victory, Defeat]
  shrink = genericShrink

instance Arbitrary Reward where
  arbitrary = Reward <$> arbitrary
  shrink = genericShrink

instance Arbitrary Event where
  arbitrary = Event <$> arbitrary
  shrink = genericShrink

instance Arbitrary EventFilter where
  arbitrary =
    frequency
      [ (3, pure Always),
        (1, pure Never),
        (5, Exactly <$> arbitrary)
      ]
  shrink = genericShrink

decayArbitrary :: Arbitrary a => Int -> Gen a
decayArbitrary n = scale (\s -> max 0 (s - n)) arbitrary

instance Arbitrary Game where
  arbitrary = sized $ \size -> do
    if size <= 1
      then elements [win, lose]
      else
        frequency
          [ (3, pure win),
            (3, pure lose),
            (3, giveReward <$> arbitrary),
            (5, andThen <$> decayArbitrary 2 <*> decayArbitrary 2),
            (5, subgame <$> decayArbitrary 3 <*> decayArbitrary 3 <*> decayArbitrary 3),
            (5, bothG <$> decayArbitrary 2 <*> decayArbitrary 2),
            (5, eitherG <$> decayArbitrary 2 <*> decayArbitrary 2),
            (5, race <$> decayArbitrary 2 <*> decayArbitrary 2),
            (5, choose <$> (elements [0, 1, 2, 3, 4] >>= \n -> replicateM n (decayArbitrary 5))),
            (2, comeback <$> arbitrary),
            (1, pure bottom),
            (5, gate <$> arbitrary <*> arbitrary)
          ]
  shrink = genericShrink

-- >>> generate $ resize 10 $ arbitrary @Game
-- GAndThen (GAndThen (Greward (Reward 5)) (GEither GLose (GAndThen (GChoose []) GWin))) GLose

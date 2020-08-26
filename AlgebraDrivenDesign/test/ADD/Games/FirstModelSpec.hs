{-# OPTIONS_GHC -Wno-orphans #-}

module ADD.Games.FirstModelSpec where

import ADD.Games.FirstModel ()
import ADD.Games.FirstModel.Events (Event (..), EventFilter, always, exactly, never)
import ADD.Games.FirstModel.Game ()
import ADD.Games.FirstModel.Result (Result (..), defeat, victory)
import ADD.Games.FirstModel.Reward (Reward (..))
import Test.Hspec
import Test.QuickCheck hiding (Result)

spec :: Spec
spec = pure ()

instance Arbitrary Result where
  arbitrary = elements [victory, defeat]
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
      [ (3, pure always),
        (1, pure never),
        (5, exactly <$> arbitrary)
      ]

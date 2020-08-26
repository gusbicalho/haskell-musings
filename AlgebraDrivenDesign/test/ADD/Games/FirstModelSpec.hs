{-# OPTIONS_GHC -Wno-orphans #-}
module ADD.Games.FirstModelSpec where

import Test.Hspec
import Test.QuickCheck hiding (Result)
import ADD.Games.FirstModel.Result (defeat, victory, Result (..))
import ADD.Games.FirstModel.Reward (Reward (..))
import ADD.Games.FirstModel.Events (exactly, always, never, EventFilter, Event (..))
import ADD.Games.FirstModel.Game ()
import ADD.Games.FirstModel ()

spec :: Spec
spec = pure ()

instance Arbitrary Result where
  arbitrary = elements [ victory, defeat ]
  shrink = genericShrink

instance Arbitrary Reward where
  arbitrary = Reward <$> arbitrary
  shrink = genericShrink

instance Arbitrary Event where
  arbitrary = Event <$> arbitrary
  shrink = genericShrink

instance Arbitrary EventFilter where
  arbitrary = frequency [ (3, pure always)
                        , (1, pure never)
                        , (5, exactly <$> arbitrary)
                        ]

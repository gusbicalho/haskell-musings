module ADD.Games.FirstModel.Reward
  ( Reward (..)
  ) where

import Data.Word (Word8)
import Data.Data (Data)
import GHC.Generics (Generic)
newtype Reward = Reward Word8
  deriving stock (Eq, Ord, Show, Data, Generic)

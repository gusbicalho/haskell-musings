module ADD_Old.Games.FirstModel.Reward
  ( Reward (..),
  )
where

import Data.Data (Data)
import Data.Word (Word8)
import GHC.Generics (Generic)

newtype Reward = Reward Word8
  deriving stock (Eq, Ord, Show, Data, Generic)

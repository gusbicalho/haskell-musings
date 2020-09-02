module ADD_Old.Games.FirstModel.Result
  ( Result (..),
  )
where

import Data.Data (Data)
import GHC.Generics (Generic)

data Result
  = Victory
  | Defeat
  deriving stock (Eq, Ord, Show, Data, Generic)

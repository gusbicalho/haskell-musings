module ADD.Games.FirstModel.Result
  ( Result (Victory, Defeat),
    victory,
    defeat,
  )
where

import Data.Data (Data)
import GHC.Generics (Generic)

data Result
  = UnsafeVictory
  | UnsafeDefeat
  deriving stock (Eq, Ord, Show, Data, Generic)

{-# COMPLETE Victory, Defeat #-}

pattern Victory :: Result
pattern Victory <- UnsafeVictory

pattern Defeat :: Result
pattern Defeat <- UnsafeDefeat

victory :: Result
victory = UnsafeVictory

defeat :: Result
defeat = UnsafeDefeat

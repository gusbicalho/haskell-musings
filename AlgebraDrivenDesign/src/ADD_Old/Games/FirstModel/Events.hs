module ADD_Old.Games.FirstModel.Events
  ( Event (..),
    EventFilter (..),
    matches,
  )
where

import Data.Data (Data)
import Data.Word (Word8)
import GHC.Generics (Generic)

newtype Event = Event Word8
  deriving stock (Eq, Ord, Show, Data, Generic)

data EventFilter
  = Always
  | Never
  | Exactly Word8
  deriving stock (Eq, Ord, Show, Data, Generic)

matches :: EventFilter -> Event -> Bool
matches Never _ = False
matches Always _ = True
matches (Exactly expected) (Event event) = expected == event

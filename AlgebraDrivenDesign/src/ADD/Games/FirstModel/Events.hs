module ADD.Games.FirstModel.Events
  ( Event (..),
    EventFilter (Always, Never, Exactly),
    always,
    never,
    exactly,
    matches,
  )
where

import Data.Data (Data)
import Data.Word (Word8)
import GHC.Generics (Generic)

newtype Event = Event Word8
  deriving stock (Eq, Ord, Show, Data, Generic)

data EventFilter
  = UnsafeAlways
  | UnsafeNever
  | UnsafeExactly Word8
  deriving stock (Eq, Ord, Show, Data, Generic)

{-# COMPLETE Always, Never, Exactly #-}

pattern Always :: EventFilter
pattern Always <- UnsafeAlways

always :: EventFilter
always = UnsafeAlways

pattern Never :: EventFilter
pattern Never <- UnsafeNever

never :: EventFilter
never = UnsafeNever

pattern Exactly :: Word8 -> EventFilter
pattern Exactly e <- UnsafeExactly e

exactly :: Word8 -> EventFilter
exactly = UnsafeExactly

matches :: EventFilter -> Event -> Bool
matches Never _ = False
matches Always _ = True
matches (Exactly expected) (Event event) = expected == event

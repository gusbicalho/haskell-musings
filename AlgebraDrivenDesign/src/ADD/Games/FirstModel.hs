{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wcompat #-}
{-# OPTIONS_GHC -Wincomplete-record-updates #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -foptimal-applicative-do #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
module ADD.Games.FirstModel where

import Data.Data (Data)
import GHC.Generics (Generic)
import Data.Word (Word8)
import Control.Monad.Trans.Writer.CPS (tell, Writer)
import Data.Foldable (find)

-- Result

data Result
  = UnsafeVictory
  | UnsafeDefeat

pattern Victory :: Result
pattern Victory <- UnsafeVictory

victory :: Result
victory = UnsafeVictory

pattern Defeat :: Result
pattern Defeat <- UnsafeDefeat

defeat :: Result
defeat = UnsafeDefeat

-- Event / EventFilter

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
matches Never  _ = False
matches Always _ = True
matches (Exactly expected) (Event event) = expected == event

-- Reward

newtype Reward = Reward Word8
  deriving stock (Eq, Ord, Show, Data, Generic)

-- Game

data Game
  = UnsafeWin
  | UnsafeLose
  | UnsafeGiveReward Reward
  | UnsafeAndThen Game Game
  | UnsafeSubgame Game Game Game
  | UnsafeEitherG Game Game
  | UnsafeBothG Game Game
  | UnsafeRace Game Game
  | UnsafeChoose [(EventFilter, Game)]
  deriving stock (Eq, Ord, Show, Data, Generic)

-- Game: Base constructors and patterns

{-# COMPLETE
    Win
  , Lose
  , GiveReward
  , AndThen
  , Subgame
  , EitherG
  , BothG
  , Race
  , Choose #-}

pattern Win :: Game
pattern Win <- UnsafeWin

win :: Game
win = UnsafeWin

pattern Lose :: Game
pattern Lose <- UnsafeLose

lose :: Game
lose = UnsafeLose

pattern GiveReward :: Reward -> Game
pattern GiveReward r <- UnsafeGiveReward r

giveReward :: Reward -> Game
giveReward = UnsafeGiveReward

pattern AndThen :: Game -> Game -> Game
pattern AndThen g1 g2 <- UnsafeAndThen g1 g2

andThen :: Game -> Game -> Game
andThen Win  _  = win -- wrong, will be fixed later
andThen Lose _  = lose
andThen g1   g2 = UnsafeAndThen g1 g2

pattern Subgame :: Game -> Game -> Game -> Game
pattern Subgame g g1 g2 <- UnsafeSubgame g g1 g2

subgame :: Game -> Game -> Game -> Game
subgame Win  g1 _  = g1
subgame Lose _  g2 = g2
subgame g    g1 g2 = UnsafeSubgame g g1 g2

pattern EitherG :: Game -> Game -> Game
pattern EitherG g1 g2 <- UnsafeEitherG g1 g2

eitherG :: Game -> Game -> Game
eitherG Lose Lose = lose
eitherG Win  _    = win
eitherG _    Win  = win
eitherG g1   g2   = UnsafeEitherG g1 g2

pattern BothG :: Game -> Game -> Game
pattern BothG g1 g2 <- UnsafeBothG g1 g2

bothG :: Game -> Game -> Game
bothG Win  Win  = win
bothG Lose _    = lose
bothG _    Lose = lose
bothG g1   g2   = UnsafeBothG g1 g2

pattern Race :: Game -> Game -> Game
pattern Race g1 g2 <- UnsafeRace g1 g2

race :: Game -> Game -> Game
race Win  _    = win
race Lose _    = lose
race _    Win  = win
race _    Lose = lose
race g1   g2   = UnsafeRace g1 g2

pattern Choose :: [(EventFilter, Game)] -> Game
pattern Choose gs <- UnsafeChoose gs

choose :: [(EventFilter, Game)] -> Game
choose = UnsafeChoose

-- Game: Derived constructors

gate :: EventFilter -> Game -> Game
gate ef game = choose [(ef, game)]

bottom :: Game
bottom = choose []

comeback :: Game -> Game
comeback game = subgame game lose win

-- Game: observations

_stepGame :: Game -> Maybe Event -> Writer [Reward] Game
_stepGame Win _ = pure win
_stepGame Lose _ = pure lose
_stepGame (GiveReward r) _ = tell [r] *> pure win
_stepGame (g1 `AndThen` g2) e =
  andThen <$> _stepGame g1 e
          <*> pure g2
_stepGame (Subgame g onWin onLose) e =
  subgame <$> _stepGame g e
          <*> pure onWin
          <*> pure onLose
_stepGame (EitherG g1 g2) e =
  eitherG <$> _stepGame g1 e
          <*> _stepGame g2 e
_stepGame (BothG g1 g2) e =
  bothG <$> _stepGame g1 e
        <*> _stepGame g2 e
_stepGame (Race g1 g2) e =
  race <$> _stepGame g1 e
       <*> _stepGame g2 e
_stepGame (Choose cs) (Just e)
  | Just (_, g) <- find (\(ef, _) -> matches ef e) cs
  = pure g
_stepGame g@(Choose _) _ = pure g

_runGame :: Game -> [Event] -> Writer [Reward] Game
_runGame g (e : evs) = do
  g <- _stepGame g (Just e)
  _runGame g evs
_runGame g [] = do
  g' <- _stepGame g Nothing
  case g == g' of
    True -> pure g
    False -> _runGame g' []

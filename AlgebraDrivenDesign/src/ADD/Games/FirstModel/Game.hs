module ADD.Games.FirstModel.Game
  ( Game ( Win
         , Lose
         , GiveReward
         , AndThen
         , Subgame
         , EitherG
         , BothG
         , Race
         , Choose )
  , win
  , lose
  , giveReward
  , andThen
  , subgame
  , eitherG
  , bothG
  , race
  , choose
  , gate
  , bottom
  , comeback
  ) where

import ADD.Games.FirstModel.Events (EventFilter)
import ADD.Games.FirstModel.Reward (Reward)
import Data.Data (Data)
import GHC.Generics (Generic)

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

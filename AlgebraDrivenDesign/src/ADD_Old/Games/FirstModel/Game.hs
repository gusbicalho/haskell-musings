module ADD_Old.Games.FirstModel.Game
  ( Game
      ( Win,
        Lose,
        GiveReward,
        AndThen,
        Subgame,
        EitherG,
        BothG,
        Race,
        Choose
      ),
    win,
    lose,
    giveReward,
    andThen,
    subgame,
    eitherG,
    bothG,
    race,
    choose,
    gate,
    bottom,
    comeback,
  )
where

import ADD_Old.Games.FirstModel.Events (EventFilter)
import ADD_Old.Games.FirstModel.Reward (Reward)
import Data.Data (Data)
import GHC.Generics (Generic)

data Game
  = GWin
  | GLose
  | GGiveReward Reward
  | GAndThen Game Game
  | GSubgame Game Game Game
  | GEither Game Game
  | GBoth Game Game
  | GRace Game Game
  | GChoose [(EventFilter, Game)]
  deriving stock (Eq, Ord, Show, Data, Generic)

-- Game: Base constructors and patterns

{-# COMPLETE
  Win,
  Lose,
  GiveReward,
  AndThen,
  Subgame,
  EitherG,
  BothG,
  Race,
  Choose
  #-}

pattern Win :: Game
pattern Win <- GWin

win :: Game
win = GWin

pattern Lose :: Game
pattern Lose <- GLose

lose :: Game
lose = GLose

pattern GiveReward :: Reward -> Game
pattern GiveReward r <- GGiveReward r

giveReward :: Reward -> Game
giveReward = GGiveReward

pattern AndThen :: Game -> Game -> Game
pattern AndThen g1 g2 <- GAndThen g1 g2

andThen :: Game -> Game -> Game
andThen Win _ = win -- wrong, will be fixed later
andThen Lose _ = lose
andThen g1 g2 = GAndThen g1 g2

pattern Subgame :: Game -> Game -> Game -> Game
pattern Subgame g g1 g2 <- GSubgame g g1 g2

subgame :: Game -> Game -> Game -> Game
subgame Win g1 _ = g1
subgame Lose _ g2 = g2
subgame g g1 g2 = GSubgame g g1 g2

pattern EitherG :: Game -> Game -> Game
pattern EitherG g1 g2 <- GEither g1 g2

eitherG :: Game -> Game -> Game
eitherG Lose Lose = lose
eitherG Win _ = win
eitherG _ Win = win
eitherG g1 g2 = GEither g1 g2

pattern BothG :: Game -> Game -> Game
pattern BothG g1 g2 <- GBoth g1 g2

bothG :: Game -> Game -> Game
bothG Win Win = win
bothG Lose _ = lose
bothG _ Lose = lose
bothG g1 g2 = GBoth g1 g2

pattern Race :: Game -> Game -> Game
pattern Race g1 g2 <- GRace g1 g2

race :: Game -> Game -> Game
race Win _ = win
race Lose _ = lose
race _ Win = win
race _ Lose = lose
race g1 g2 = GRace g1 g2

pattern Choose :: [(EventFilter, Game)] -> Game
pattern Choose gs <- GChoose gs

choose :: [(EventFilter, Game)] -> Game
choose = GChoose

-- Game: Derived constructors

gate :: EventFilter -> Game -> Game
gate ef game = choose [(ef, game)]

bottom :: Game
bottom = choose []

comeback :: Game -> Game
comeback game = subgame game lose win

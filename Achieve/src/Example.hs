{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Example (main) where

import Achieve
import Achieve.Describe (describeGame)
import Data.Function ((&))
import Data.Map qualified as Map
import Data.MultiSet qualified as MultiSet
import Data.Set qualified as Set
import Data.Text qualified as T

main :: IO ()
main = do
  putStrLn (T.unpack (describeGame game))

game :: Game
game =
  MkGame
    { board = board
    , playerOrder = ["cleiton", "robson"]
    , standardActions = ["skip"]
    , actions =
        actionsList
          [ MkAction
              { actionId = "skip"
              , description = "Think for one turn."
              , runAction = pure
              }
          ]
    }

board :: GameBoard
board =
  MkGameBoard
    { players =
        playersList
          [ robson
          , cleiton
          ]
    , groupAwards = []
    , opportunities =
        MultiSet.fromList
          [ openSourceContribution
          , projectDelivered
          , projectDelivered
          , eoyBonus
          ]
    , deck =
        [ rsus
        , projectDelivered
        , eoyBonus
        , openSourceContribution
        ]
    }
    & playerAchieves "robson" rsus
    & playerAchieves "robson" projectDelivered
    & playerAchieves "robson" projectDelivered
    & playerAchieves "robson" openSourceContribution
    & playerAchieves "cleiton" projectDelivered
    & playerAchieves "cleiton" eoyBonus

robson :: Player
robson =
  newPlayer "robson" restAndVest
    & addEnergy (MkEnergy 3)
    & becomeAnnoyedAt "cleiton"

cleiton :: Player
cleiton =
  newPlayer "cleiton" madReps
    & addEnergy (MkEnergy 5)
    & incGoodwillFrom "robson"

-- Goals
madReps :: Goal
madReps =
  MkGoal
    { name = "mad reps"
    , requirements =
        [ ("industry reputation", AtLeast 6)
        , ("company reputation", AtLeast 3)
        ]
    }

restAndVest :: Goal
restAndVest =
  MkGoal
    { name = "rest and vest"
    , requirements =
        [ ("cash", AtLeast 30)
        , ("stress", AtMost 5)
        ]
    }

-- Achievements

eoyBonus :: Achievement
eoyBonus =
  MkAchievement
    { name = "End-of-Year Bonus"
    , groupAwards = []
    , playerAwards = [("cash", 5)]
    , cost = MkEnergy 5
    }

rsus :: Achievement
rsus =
  MkAchievement
    { name = "RSU Grant"
    , groupAwards = []
    , playerAwards = [("cash", 10)]
    , cost = MkEnergy 5
    }

openSourceContribution :: Achievement
openSourceContribution =
  MkAchievement
    { name = "Open Source Contribution"
    , groupAwards = []
    , playerAwards =
        [ ("industry reputation", 2)
        , ("stress", 1)
        ]
    , cost = MkEnergy 2
    }

projectDelivered :: Achievement
projectDelivered =
  MkAchievement
    { name = "Project Delivered"
    , groupAwards =
        [ ("company reputation", 1)
        , ("kpi", 1)
        ]
    , playerAwards =
        [ ("cash", 1)
        , ("company reputation", 1)
        , ("stress", 1)
        ]
    , cost = MkEnergy 2
    }

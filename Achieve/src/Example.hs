{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Example (main) where

import Achieve
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

robson :: Player
robson =
  MkPlayer
    { playerId = "robson"
    , goal = restAndVest
    , inventory =
        MkInventory
          { attention = MkAttention 3
          , annoyedAt =
              Set.fromList
                [ "cleiton"
                ]
          , goodwillFor =
              MultiSet.fromList []
          , assets =
              MultiSet.fromList
                [ rsus
                , projectDelivered
                , projectDelivered
                , openSourceContribution
                ]
          }
    }

cleiton :: Player
cleiton =
  MkPlayer
    { playerId = "cleiton"
    , goal = madReps
    , inventory =
        MkInventory
          { attention = MkAttention 5
          , annoyedAt = mempty
          , goodwillFor =
              MultiSet.fromList ["robson"]
          , assets =
              MultiSet.fromList
                [ eoyBonus
                , projectDelivered
                ]
          }
    }

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

-- Assets

eoyBonus :: Asset
eoyBonus =
  MkAsset
    { name = "End-of-Year Bonus"
    , awards = [("cash", 5)]
    }

rsus :: Asset
rsus =
  MkAsset
    { name = "RSU Grant"
    , awards = [("cash", 10)]
    }

openSourceContribution :: Asset
openSourceContribution =
  MkAsset
    { name = "Open Source Contribution"
    , awards =
        [ ("industry reputation", 2)
        , ("stress", 1)
        ]
    }

projectDelivered :: Asset
projectDelivered =
  MkAsset
    { name = "Project Delivered"
    , awards =
        [ ("cash", 1)
        , ("company reputation", 1)
        , ("stress", 1)
        ]
    }

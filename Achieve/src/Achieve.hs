{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Achieve where

import Data.Foldable qualified as F
import Data.Function ((&))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.MultiSet (MultiSet)
import Data.MultiSet qualified as MultiSet
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T

data Game = MkGame
  { board :: GameBoard
  , standardActions :: Set ActionId
  , actions :: Map ActionId Action
  , playerOrder :: [PlayerId]
  }

data Action = MkAction
  { actionId :: ActionId
  , description :: Text
  , runAction :: GameBoard -> IO GameBoard
  }

data GameBoard = MkGameBoard
  { opportunities :: Opportunities
  , groupAwards :: Map Award Int
  , players :: Map PlayerId Player
  , deck :: [Achievement]
  }

type Opportunities = MultiSet Achievement

data Player = MkPlayer
  { playerId :: PlayerId
  , goal :: Goal
  , inventory :: Inventory
  }

newtype PlayerId = MkPlayerId {get :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)

newtype ActionId = MkActionId {get :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)

data Goal = MkGoal
  { name :: Text
  , requirements :: Map Award Requirement
  }
  deriving stock (Eq, Ord, Show)

data Requirement
  = AtLeast Int
  | AtMost Int
  | Exactly Int
  deriving stock (Eq, Ord, Show)

data Inventory = MkInventory
  { energy :: Energy
  , annoyedAt :: Set PlayerId
  , goodwillFrom :: MultiSet PlayerId
  , awards :: Map Award Int
  , achievements :: MultiSet AchievementName
  }

newtype Energy = MkEnergy {get :: Word}
  deriving stock (Eq, Ord, Show)

data Achievement = MkAchievement
  { name :: AchievementName
  , playerAwards :: Map Award Int
  , groupAwards :: Map Award Int
  , cost :: Energy
  }
  deriving stock (Eq, Ord, Show)

newtype AchievementName = MkAchievementName {get :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)

newtype Award = MkAward {get :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)

-- Helpers

newPlayer :: PlayerId -> Goal -> Player
newPlayer playerId goal =
  MkPlayer
    { playerId
    , goal
    , inventory =
        MkInventory
          { energy = MkEnergy 0
          , annoyedAt = mempty
          , goodwillFrom = mempty
          , awards = mempty
          , achievements = mempty
          }
    }

playersList :: [Player] -> Map PlayerId Player
playersList = Map.fromList . fmap (\p -> (p.playerId, p))

actionsList :: [Action] -> Map ActionId Action
actionsList = Map.fromList . fmap (\p -> (p.actionId, p))

-- Basic operations

addEnergy :: Energy -> Player -> Player
addEnergy energy player@MkPlayer{inventory} =
  player
    { inventory =
        inventory
          { energy = MkEnergy $ player.inventory.energy.get + energy.get
          }
    }

spendEnergy :: Energy -> Player -> Player
spendEnergy energy player@MkPlayer{inventory} =
  player
    { inventory =
        inventory
          { energy = MkEnergy $ player.inventory.energy.get + energy.get
          }
    }

becomeAnnoyedAt :: PlayerId -> Player -> Player
becomeAnnoyedAt playerId player =
  player
    { inventory =
        player.inventory
          { annoyedAt = Set.insert playerId player.inventory.annoyedAt
          }
    }

incGoodwillFrom :: PlayerId -> Player -> Player
incGoodwillFrom playerId player =
  player
    { inventory =
        player.inventory
          { goodwillFrom = MultiSet.insert playerId player.inventory.goodwillFrom
          }
    }

overPlayer :: PlayerId -> (Player -> Player) -> GameBoard -> GameBoard
overPlayer playerId f board =
  board{players = Map.adjust f playerId board.players}

grantGroupAwards :: Map Award Int -> GameBoard -> GameBoard
grantGroupAwards awards board =
  board
    { groupAwards = Map.unionWith (+) board.groupAwards awards
    , deck = board.deck
    }

grantPlayerAwards :: Map Award Int -> Player -> Player
grantPlayerAwards awards player =
  player
    { inventory =
        player.inventory
          { awards = Map.unionWith (+) player.inventory.awards awards
          }
    }

grantPlayerAchievement :: Achievement -> Player -> Player
grantPlayerAchievement achievement player =
  player
    { inventory =
        player.inventory
          { achievements = MultiSet.insert achievement.name player.inventory.achievements
          }
    }
    & grantPlayerAwards achievement.playerAwards

-- High level ops

playerAchieves :: PlayerId -> Achievement -> GameBoard -> GameBoard
playerAchieves playerId achievement board =
  board
    & grantGroupAwards achievement.groupAwards
    & overPlayer playerId (grantPlayerAchievement achievement)

-- playerTakesOpportunity
-- If player has enough energy, they spend it to pay the cost of an
-- opportunity on the board. The opportunity gets removed, and playerAchieves
-- that Achievement.

-- playerGetsHelpFrom
-- Player A takes 1 energy from Player B, which A must spend in this turn.
-- A cannot do this if B is Annoyed at A.
-- If Player A has Goodwill from B, they may spend 1 goodwill.
-- If they don't or can't, B becomes Annoyed at A.

-- playerForgives
-- If Player A is Annoyed at B, A can stop being Annoyed at B and get 1
-- Goodwill from B.

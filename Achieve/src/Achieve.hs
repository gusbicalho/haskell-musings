{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Achieve where

import Data.Foldable qualified as F
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
  , players :: Map PlayerId Player
  , deck :: [Asset]
  }

playersList :: [Player] -> Map PlayerId Player
playersList = Map.fromList . fmap (\p -> (p.playerId, p))

actionsList :: [Action] -> Map ActionId Action
actionsList = Map.fromList . fmap (\p -> (p.actionId, p))

type Opportunities = MultiSet Asset

data Player = MkPlayer
  { playerId :: PlayerId
  , playerRole :: PlayerRole
  , goal :: Goal
  , inventory :: Inventory
  }

newtype PlayerId = MkPlayerId Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)

getPlayerId :: PlayerId -> Text
getPlayerId (MkPlayerId t) = t

newtype ActionId = MkActionId Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)

getActionId :: ActionId -> Text
getActionId (MkActionId t) = t

data PlayerRole = MkPlayerRole
  { name :: Text
  , specialActions :: Set ActionId
  }
  deriving stock (Eq, Ord, Show)

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
  { attention :: Attention
  , annoyedAt :: Set PlayerId
  , goodwillFor :: MultiSet PlayerId
  , assets :: MultiSet Asset
  }

newtype Attention = MkAttention Word
  deriving stock (Eq, Ord, Show)

getAttention :: Attention -> Word
getAttention (MkAttention t) = t

data Asset = MkAsset
  { name :: Text
  , awards :: Map Award Int
  }
  deriving stock (Eq, Ord, Show)

newtype Award = MkAward Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)

getAward :: Award -> Text
getAward (MkAward t) = t

-- Describe the game

describeGame :: Game -> Text
describeGame game =
  renderLayout
    [board, Line "", todo]
 where
  ind = Ind "  "
  board =
    Group
      [ Group (player <$> Map.elems game.board.players)
      , "Opportunities:"
      , ind (assets <$> MultiSet.toAscOccurList game.board.opportunities)
      , ""
      , Line $ "Deck: " <> T.pack (show $ length game.board.deck) <> " cards"
      ]
  assets (asset, count) =
    Group . concat . replicate count $
      [ Line $ asset.name
      , ind (describeAward <$> Map.toAscList asset.awards)
      ]
  describeAward (MkAward award, num) =
    Line $ (T.pack $ show num) <> " " <> award
  player (p :: Player) =
    Group
      [ Line $ (getPlayerId p.playerId) <> " - " <> p.playerRole.name
      , ind [inventory p.inventory]
      , ""
      ]
  inventory i =
    Group
      [ Line $ "Attention: " <> (T.pack . show $ getAttention i.attention)
      , Line $ "Annoyed at " <> (T.intercalate ", " . map getPlayerId $ Set.toAscList i.annoyedAt)
      , Line $ "Goodwill for:"
      , ind ((\(MkPlayerId pid, n) -> Line $ pid <> ": " <> T.pack (show n)) <$> MultiSet.toOccurList i.goodwillFor)
      , "Assets:"
      , ind (assets <$> MultiSet.toAscOccurList i.assets)
      ]
  todo = case game.playerOrder of
    [] -> Group []
    nextPlayerId : _ ->
      Group
        [ Line $ "Next player: " <> getPlayerId nextPlayerId
        , "Actions:"
        , ind (action <$> Set.toAscList game.standardActions)
        , ind do
            player <- F.toList $ Map.lookup nextPlayerId game.board.players
            specialAction <- F.toList player.playerRole.specialActions
            pure (action specialAction)
        ]
  action actionId =
    Group do
      action <- F.toList (Map.lookup actionId game.actions)
      [Line (getActionId action.actionId), ind [Line action.description]]

data Layout
  = Line Text
  | Group [Layout]
  | Ind Text [Layout]

instance IsString Layout where
  fromString = Line . T.pack

renderLayout :: [Layout] -> Text
renderLayout = goAll ""
 where
  goAll ind layouts = T.concat (goEach ind <$> layouts)
  goEach ind (Line text) = ind <> text <> "\n"
  goEach ind (Group layouts) = goAll ind layouts
  goEach ind (Ind addInd layouts) = goAll (ind <> addInd) layouts

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Achieve.Describe (describeGame) where

import Achieve (Achievement, ActionId, Award, Game, GameBoard, Inventory, Player)
import Achieve qualified
import Data.Foldable qualified as F
import Data.Functor ((<&>))
import Data.Map qualified as Map
import Data.MultiSet qualified as MultiSet
import Data.Set qualified as Set
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Exts (IsString (..))

describeGame :: Game -> Text
describeGame game =
  renderLayout $
    Group
      [ describeBoard game.board
      , Line ""
      , describeTodo game
      ]

describeBoard :: GameBoard -> Layout
describeBoard board =
  Group
    [ Group $ describePlayer <$> Map.elems board.players
    , ""
    , "Group awards:"
    , ind $ describeAward <$> Map.toAscList board.groupAwards
    , ""
    , "Opportunities:"
    , ind (describeAchievement <$> MultiSet.toList board.opportunities)
    , ""
    , Line $ "Deck: " <> T.pack (show $ length board.deck) <> " cards"
    ]

describeTodo :: Game -> Layout
describeTodo game = case game.playerOrder of
  [] -> Group []
  nextPlayerId : _ ->
    Group
      [ Line $ "Next player: " <> nextPlayerId.get
      , "Actions:"
      , ind (describeAction game <$> Set.toAscList game.standardActions)
      ]

describeAchievement :: Achievement -> Layout
describeAchievement achievement =
  Group
    [ Line $
        achievement.name.get
          <> " - Cost: "
          <> T.pack (show achievement.cost.get)
          <> " energy"
    , ind (describeAward <$> Map.toAscList achievement.playerAwards)
    ]

describeAward :: (Award, Int) -> Layout
describeAward (award, num) =
  Line $ (T.pack $ show num) <> " " <> award.get

describePlayer :: Player -> Layout
describePlayer p =
  Group
    [ Line $ p.playerId.get
    , ind [describeInventory p.inventory]
    , ""
    ]

describeInventory :: Inventory -> Layout
describeInventory i =
  Group
    [ Line $ "Energy: " <> (T.pack . show $ i.energy.get)
    , Line $ "Annoyed at:"
    , ind $ Line . (\a -> a.get) <$> Set.toAscList i.annoyedAt
    , Line $ "Goodwill from:"
    , ind $
        MultiSet.toOccurList i.goodwillFrom <&> \(pid, n) ->
          Line $ pid.get <> ": " <> T.pack (show n)
    , "Awards:"
    , ind $
        Map.toAscList i.awards <&> \(award, val) ->
          Line $ award.get <> ": " <> T.pack (show val)
    , "Achievements:"
    , ind $
        MultiSet.toAscOccurList i.achievements <&> \(achievement, count) ->
          Line $ (T.pack $ show count) <> "x " <> achievement.get
    ]

describeAction :: Game -> ActionId -> Layout
describeAction game actionId =
  Group do
    action <- F.toList (Map.lookup actionId game.actions)
    [ Line action.actionId.get
      , ind [Line action.description]
      ]

-- text layout
data Layout
  = Line Text
  | Group [Layout]
  | Ind Text [Layout]

instance IsString Layout where
  fromString = Line . T.pack

ind :: [Layout] -> Layout
ind = Ind "  "

renderLayout :: Layout -> Text
renderLayout = goEach ""
 where
  goAll ind layouts = T.concat (goEach ind <$> layouts)
  goEach ind (Line text) = ind <> text <> "\n"
  goEach ind (Group layouts) = goAll ind layouts
  goEach ind (Ind addInd layouts) = goAll (ind <> addInd) layouts

module ADD.Games.FirstModel where

import ADD.Games.FirstModel.Events (Event (..), exactly, matches)
import ADD.Games.FirstModel.Game
import ADD.Games.FirstModel.Result (Result, defeat, victory)
import ADD.Games.FirstModel.Reward (Reward (..))
import Control.Monad.Trans.Writer.CPS (Writer, runWriter, tell)
import Data.Foldable (find)
import Data.Functor ((<&>))
import Data.List (transpose)
import Data.Tuple (swap)

-- Game: observations

_stepGame :: Game -> Maybe Event -> Writer [Reward] Game
_stepGame Win _ = pure win
_stepGame Lose _ = pure lose
_stepGame (GiveReward r) _ = tell [r] *> pure win
_stepGame (g1 `AndThen` g2) e =
  andThen
    <$> _stepGame g1 e
    <*> pure g2
_stepGame (Subgame g onWin onLose) e =
  subgame
    <$> _stepGame g e
    <*> pure onWin
    <*> pure onLose
_stepGame (EitherG g1 g2) e =
  eitherG
    <$> _stepGame g1 e
    <*> _stepGame g2 e
_stepGame (BothG g1 g2) e =
  bothG
    <$> _stepGame g1 e
    <*> _stepGame g2 e
_stepGame (Race g1 g2) e =
  race
    <$> _stepGame g1 e
    <*> _stepGame g2 e
_stepGame (Choose cs) (Just e)
  | Just (_, g) <- find (\(ef, _) -> matches ef e) cs =
    pure g
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

_toResult :: Game -> Maybe Result
_toResult Win = Just victory
_toResult Lose = Just defeat
_toResult _ = Nothing

runGame :: [Event] -> Game -> ([Reward], Maybe Result)
runGame evs g =
  swap . runWriter . fmap _toResult $
    _runGame g evs

-- examples
bingo :: [[Game]] -> Reward -> Game
bingo gamesTable reward =
  eitherG
    (anyFullRow gamesTable)
    (anyFullRow $ transpose gamesTable)
    `andThen` giveReward reward
  where
    anyOf games = foldr eitherG lose games
    allOf games = foldr bothG win games
    anyFullRow gamesTable = anyOf (fmap allOf gamesTable)

bingo_game :: Game
bingo_game = bingo slots (Reward 100)
  where
    slots =
      [0 .. 2] <&> \x ->
        [0 .. 2] <&> \y ->
          gate (exactly $ x * 10 + y) win

-- >>> runGame [] bingo_game
-- ([],Nothing)
-- >>> runGame [Event 0, Event 1] bingo_game
-- ([],Nothing)
-- >>> runGame [Event 0, Event 1, Event 2] bingo_game
-- ([],Just UnsafeVictory)
-- >>> runGame [Event 1, Event 0, Event 2] bingo_game
-- ([],Just UnsafeVictory)
-- >>> runGame [Event 1, Event 11, Event 21] bingo_game
-- ([],Just UnsafeVictory)

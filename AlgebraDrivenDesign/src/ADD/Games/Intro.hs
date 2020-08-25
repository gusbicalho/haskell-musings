module ADD.Games.Intro where

{-
The scenarios available in the first version of the game were linear sequences;
do A, then B, and then finally C. In some particularly exotic scenarios, you
could complete them in any order.

The number of required tasks would vary.

Sometimes you’d get small rewards for partial completion, but some scenarios
would only give you one large reward at the end.

The mythical über-implementation would be capable of supporting bingo-like
games: ones with multiple overlapping sub-games, any of which could be won on
its own.
-}
data Scenario = S
data Reward = R

andThen :: Scenario -> Scenario -> Scenario
andThen = const

bigGame =
  runGameWith stories $
    completeStory "dance"
      `andS` completeStory "climb a tree"
  where
    stories = [ ("dance", danceStory)
              , ("climb a tree", climbTreeStory)
              ]
    danceStory = dance $$$ 10
    climbTreeStory = (
        (
          climbTree $$$ 10
        ) `thenS` (
          (jumpDown $$$ 10)
          `orS` climbDown
          `orS` (
            (completeStory "dance" $$$ 5)
              `thenS` climbDown
          )
        )
      ) $$$ 50
    runGameWith = undefined
    -- Combinators
    s `thenS` _ = S
    s `orS` _ = S
    s `andS` _ = S
    story storyName = undefined
    earn :: Reward -> Scenario
    earn reward = undefined
    scenario $$$ i = scenario `andThen` earn (points i)
    -- Primitives
    points i = R
    completeStory _ = S
    dance = S
    climbTree = S
    climbDown = S
    jumpDown = S


{-
Exercise:
  Sketch the core data types and functions necessary for a game-engine
  design that solves the above problems. Under your design, how would you
  represent a game that requires a player to do X, then Y, and then receive a
  reward?
-}

{-
Exercise:
  Try to represent a game that requires players to do X and Y — but in either
  order — and then receive a reward. Can your initial design represent this
  problem? If not, modify the core data types to support this functionality.
-}

{-
Exercise:
  What implications does your design have, in terms of its eventual usability,
  performance, complexity, and extensibility?
-}

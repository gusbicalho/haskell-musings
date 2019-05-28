{-# LANGUAGE
  ExistentialQuantification,
  FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses,
  RankNTypes
#-}
module FiniteAutomata where

  type Transition s = s -> [s]
  class ToTransition t s | t -> s where
    toTransition :: t -> Transition s
  instance ToTransition (Transition s) s where
    toTransition = id

  dead :: Transition s
  dead = const []

  (-:-) :: (ToTransition p s, ToTransition q s) => p -> q -> Transition s
  t -:- u = \s -> (toTransition t) s <> (toTransition u) s
  infixr 3 -:-

  instance (Eq s) => ToTransition (s, [s]) s where
    toTransition (expect, dest) s = [d | d <- dest, s == expect]

  type TransitionTable s i = [(s, i, s)]
  transitions :: (Eq s, Eq i) => TransitionTable s i -> i -> Transition s
  transitions table i s = [dest | (start, input, dest) <- table,
                                  start == s, input == i]

  process :: (Eq s, Eq i) => TransitionTable s i -> [s] -> [i] -> [s]
  process table states inputs = foldl (>>=) states $ map tt inputs
    where tt = transitions table

  -- example below

  data State = Initial | Second | Third deriving (Eq, Show)
  data Input = A | B | C deriving (Eq, Show)

  table = [(Initial, A, Initial)
          ,(Initial, B, Second)
          ,(Initial, B, Third)
          ,(Initial, C, Third)
          ,(Second,  A, Second)
          ,(Second,  B, Second)
          ,(Second,  C, Initial)
          ,(Second,  C, Third)
          ,(Third,   C, Third)
          ]
  tt = transitions table

  a = foldl (-:-) dead [(Initial, [Initial])
                       ,(Second,  [Second])]

  b =   (Initial, [Second
                  ,Third])
    -:- (Second,  [Second])

  c =   (Initial, [Third  ])
    -:- (Second,  [Initial
                  ,Third  ])
    -:- (Third,   [Third  ])

  tests = [ [Initial] >>= a >>= b >>= a >>= c >>= c
          , foldl (>>=) [Initial] [a, b, a, c, c]
          , [Initial] >>= tt A >>= tt B >>= tt A >>= tt C >>= tt C
          , foldl (>>=) [Initial] $ map tt [A, B, A, C, C]
          , process table [Initial] [A, B, A, C, C]
          ]
  -- >>> all ([Third,Third] ==) tests
  -- True
  --

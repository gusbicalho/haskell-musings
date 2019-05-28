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

  -- examples below

  data State = Initial | Second | Third deriving (Eq, Show)

  a = foldl (-:-) dead [(Initial, [Initial])
                       ,(Second,  [Second])]

  b =   (Initial, [Second
                  ,Third])
    -:- (Second,  [Second])

  c =   (Initial, [Third  ])
    -:- (Second,  [Initial
                  ,Third  ])
    -:- (Third,   [Third  ])

{--
>>> [Initial] >>= a >>= b >>= a >>= c >>= c
[Third,Third]

>>> foldl (>>=) [Initial] [a, b, a, c, c]
[Third,Third]

--}
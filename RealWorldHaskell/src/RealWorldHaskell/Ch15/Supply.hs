{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RealWorldHaskell.Ch15.Supply
  ( Supply
  , runSupply
  , next
  ) where

import Control.Monad.State (State, get, put, runState)

newtype Supply s a = Sup (State [s] a)
  deriving (Functor, Applicative, Monad)

runSupply :: Supply s a -> [s] -> (a, [s])
runSupply (Sup s) = runState s

next :: Supply s (Maybe s)
next = Sup $ do
  xs <- get
  case xs of
    []        ->            return Nothing
    (x : xs') -> put xs' >> return (Just x)

-- >>> runSupply ((fmap Data.Char.toUpper) <$> next) "qwertyuiop"
-- (Just 'Q',"wertyuiop")
--

{-
--Handmade version
newtype Supply s a = Supply ([s] -> (a, [s]))

runSupply :: Supply s a -> [s] -> (a, [s])
runSupply (Supply s) = s

instance Functor (Supply s) where
  fmap f s = Supply $ mapfst f . runSupply s
    where mapfst g (a, b) = (g a, b)

instance Monad (Supply s) where
  return a = Supply $ \xs -> (a, xs)
  sa >>= f = Supply bound
    where bound xs = let (a, ys) = runSupply sa xs
                     in runSupply (f a) ys

instance Applicative (Supply s) where
  pure = return
  (<*>) = ap

next :: Supply s (Maybe s)
next = Supply next'
  where next' []     = (Nothing, [])
        next' (x:xs) = (Just x,  xs)
-}

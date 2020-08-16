{-# LANGUAGE
    FlexibleInstances
  , FunctionalDependencies
  , MultiParamTypeClasses
  , NoMonomorphismRestriction
#-}
module RealWorldHaskell.Ch15.SupplyClass
  ( MonadSupply(..)
  , S.runSupply
  , S.Supply
  , nextPair
  ) where

import qualified RealWorldHaskell.Ch15.Supply as S
import Control.Monad(liftM2)

class Monad m => MonadSupply s m | m -> s where
  next :: m (Maybe s)

instance MonadSupply s (S.Supply s) where
  next = S.next

nextPair :: (MonadSupply s m) => m (Maybe (s, s))
nextPair = (liftM2 . liftM2) (,) next next

-- >>> S.runSupply nextPair $ "abcd"
-- (Just ('a','b'),"cd")
--

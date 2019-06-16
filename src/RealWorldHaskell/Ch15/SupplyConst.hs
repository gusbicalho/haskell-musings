{-# LANGUAGE
    FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
#-}
module RealWorldHaskell.Ch15.SupplyConst where

import qualified RealWorldHaskell.Ch15.SupplyClass as S
import Control.Monad.Reader

newtype SupplyConst e a = SC { runSC :: Reader e a }
  deriving (Functor, Applicative, Monad)

instance S.MonadSupply e (SupplyConst e) where
  next = SC $ asks Just

runSupplyConst :: SupplyConst e a -> e -> a
runSupplyConst = runReader . runSC
-- >>> runSupplyConst S.nextPair "env"
-- Just ("env","env")
--

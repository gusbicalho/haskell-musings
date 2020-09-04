{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Hand-crafted Cont (delimited continuation) monad
module Cont where

import Control.Monad (ap, liftM)

newtype Cont r a = Cont {runCont :: (a -> r) -> r}

cont :: ((a -> r) -> r) -> Cont r a
cont = Cont

instance Functor (Cont r) where
  fmap = liftM

instance Applicative (Cont r) where
  pure a = Cont $ \k -> k a
  (<*>) = ap

instance Monad (Cont r) where
  (>>=) :: forall a b. Cont r a -> (a -> Cont r b) -> Cont r b
  (Cont continueProvidingA) >>= f = Cont $
    \(completeExpectingB :: b -> r) ->
      continueProvidingA $ \a ->
        let (Cont continueProvidingB) = f a
         in continueProvidingB completeExpectingB

ex :: Cont String Integer
ex = do
  a <- pure 1
  b <- cont (\k -> k 10 <> k 20)
  pure $ a + b

-- >>> runCont ex show
-- "1121"

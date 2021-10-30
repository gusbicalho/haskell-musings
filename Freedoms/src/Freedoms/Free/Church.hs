{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Freedoms.Free.Church (Free, free, run) where

import Control.Applicative (liftA)
import Control.Monad (ap)

newtype Free f a where
  Free :: (forall r. (a -> r) -> (f (Free f a) -> r) -> r) -> Free f a

{-# INLINE lift #-}
lift :: f (Free f a) -> Free f a
lift fa = Free $ \_onPure onLift -> onLift fa

{-# INLINE free #-}
free :: Functor f => f a -> Free f a
free fa = lift (fmap pure fa)

instance Functor f => Functor (Free f) where
  {-# INLINE fmap #-}
  fmap = liftA

instance Functor f => Applicative (Free f) where
  {-# INLINE pure #-}
  pure a = Free $ \onPure _onLift -> onPure a
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Functor f => Monad (Free f) where
  {-# INLINE (>>=) #-}
  Free runMa >>= mkMb = runMa onPure onLift
   where
    onPure a = mkMb a
    onLift fa = lift (fmap (>>= mkMb) fa)

{-# INLINE run #-}
run :: forall f a m. (Monad m) => (forall x. f x -> m x) -> Free f a -> m a
run interpret = go
 where
  go (Free runChurch) = runChurch onPure onLift
  onPure = pure
  onLift fa = go =<< interpret fa

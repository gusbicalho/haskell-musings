{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Freedoms.Free.Data (Free, free, run) where

import Control.Applicative (liftA)
import Control.Monad (ap)
import Data.Kind (Type)

type Free :: (Type -> Type) -> (Type -> Type)
data Free f a where
  Pure :: a -> Free f a
  Lift :: f (Free f a) -> Free f a

instance Functor f => Functor (Free f) where
  {-# INLINE fmap #-}
  fmap = liftA

instance Functor f => Applicative (Free f) where
  {-# INLINE pure #-}
  pure = Pure
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Functor f => Monad (Free f) where
  {-# INLINE (>>=) #-}
  ma >>= mkMb = case ma of
    Pure a -> mkMb a
    Lift fa -> Lift (fmap (>>= mkMb) fa)

{-# INLINE free #-}
free :: Functor f => f a -> Free f a
free = Lift . fmap pure

{-# INLINE run #-}
run :: forall f a m. (Monad m) => (forall x. f x -> m x) -> Free f a -> m a
run interpret = go
 where
  go (Pure a) = pure a
  go (Lift fa) = go =<< interpret fa

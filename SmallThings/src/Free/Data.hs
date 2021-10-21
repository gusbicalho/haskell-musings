{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Free.Data where

import Control.Applicative (liftA)
import Control.Monad (ap)
import Data.Kind (Type)

-- Free, initial

type Free :: (Type -> Type) -> (Type -> Type)
data Free f a where
  FreePure :: a -> Free f a
  FreeLift :: f (Free f a) -> Free f a

instance Functor f => Functor (Free f) where
  fmap = liftA

instance Functor f => Applicative (Free f) where
  pure = FreePure
  (<*>) = ap

instance Functor f => Monad (Free f) where
  ma >>= mkMb = case ma of
    FreePure a -> mkMb a
    FreeLift fa -> FreeLift (fmap (>>= mkMb) fa)

lift :: f (Free f a) -> Free f a
lift = FreeLift

run :: forall f a m. (Monad m) => (forall x. f x -> m x) -> Free f a -> m a
run interpret = go
 where
  go (FreePure a) = pure a
  go (FreeLift fa) = go =<< interpret fa

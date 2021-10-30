{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Freedoms.Free.FinalClassy (Free, Interpreter (..), free, run) where

import Control.Applicative (liftA)
import Control.Monad (ap)
import Data.Kind (Type)

type Free :: (Type -> Type) -> (Type -> Type)
newtype Free f a where
  Free :: {run :: forall m. Interpreter f m => m a} -> Free f a

class Monad m => Interpreter f m where
  interpret :: f a -> m a

{-# INLINE free #-}
free :: f a -> Free f a
free fa = Free $ interpret fa

instance Functor (Free f) where
  {-# INLINE fmap #-}
  fmap = liftA

instance Applicative (Free f) where
  {-# INLINE pure #-}
  pure a = Free (pure a)
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Monad (Free f) where
  {-# INLINE (>>=) #-}
  Free ma >>= mkMb = Free $ do
    a <- ma
    run $ mkMb a

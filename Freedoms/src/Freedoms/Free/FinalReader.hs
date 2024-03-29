{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Freedoms.Free.FinalReader (Free, free, run) where

import Control.Applicative (liftA)
import Control.Monad (ap)
import Control.Monad.Trans.Reader (ReaderT (ReaderT))
import Data.Kind (Type)

newtype Interpreted f m a where
  Interpreted :: {runInterpreted :: (forall x. f x -> m x) -> m a} -> Interpreted f m a
  deriving (Functor, Applicative, Monad) via (ReaderT (forall x. f x -> m x) m)

type Free :: (Type -> Type) -> (Type -> Type)
newtype Free f a where
  Free :: {runMF :: forall m. Monad m => Interpreted f m a} -> Free f a

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
    runMF $ mkMb a

{-# INLINE free #-}
free :: f a -> Free f a
free fa = Free $ Interpreted $ \interpret -> interpret fa

{-# INLINE run #-}
run :: forall f a m. (Monad m) => (forall x. f x -> m x) -> Free f a -> m a
run interpret ma = runInterpreted (runMF ma) interpret

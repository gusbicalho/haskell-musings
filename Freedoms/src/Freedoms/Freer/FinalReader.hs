{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Freedoms.Freer.FinalReader (Freer, freer, run) where

import Control.Applicative (liftA)
import Control.Monad (ap)
import Control.Monad.Trans.Reader (ReaderT (ReaderT))
import Data.Kind (Type)
import Freedoms.Freer.Common (Interpret, RunCont)

newtype Interpreted f m a where
  Interpreted :: {runInterpreted :: (forall x. f (Freer f) x -> m x) -> m a} -> Interpreted f m a
  deriving (Functor, Applicative, Monad) via (ReaderT (forall x. f (Freer f) x -> m x) m)

type Freer :: ((Type -> Type) -> Type -> Type) -> (Type -> Type)
newtype Freer f a where
  Freer :: {runMF :: forall m. Monad m => Interpreted f m a} -> Freer f a

instance Functor (Freer f) where
  {-# INLINE fmap #-}
  fmap = liftA

instance Applicative (Freer f) where
  {-# INLINE pure #-}
  pure a = Freer (pure a)
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Monad (Freer f) where
  {-# INLINE (>>=) #-}
  Freer ma >>= mkMb = Freer $ do
    a <- ma
    runMF $ mkMb a

{-# INLINE freer #-}
freer :: f (Freer f) a -> Freer f a
freer fa = Freer $ Interpreted ($ fa)

{-# INLINE run #-}
run ::
  Monad m =>
  (RunCont (Freer f) f -> Interpret (Freer f) f m) ->
  Freer f a ->
  m a
run mkInterpret = runIt (mkInterpret runIt)
 where
  {-# INLINE runIt #-}
  runIt :: RunCont (Freer f) f
  runIt interpret ma =
    runInterpreted (runMF ma) interpret

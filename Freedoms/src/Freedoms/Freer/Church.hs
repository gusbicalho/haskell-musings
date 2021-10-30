{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Freedoms.Freer.Church (Freer, freer, run) where

import Control.Applicative (liftA)
import Control.Monad (ap)
import Data.Kind (Type)
import Freedoms.Freer.Common (Interpret, RunCont)

type Freer :: ((Type -> Type) -> Type -> Type) -> (Type -> Type)
newtype Freer f a where
  Freer :: (forall r. (a -> r) -> (forall e. f (Freer f) e -> (e -> Freer f a) -> r) -> r) -> Freer f a

instance Functor (Freer f) where
  {-# INLINE fmap #-}
  fmap = liftA

instance Applicative (Freer f) where
  {-# INLINE pure #-}
  pure a = Freer $ \onPure _onBind -> onPure a

  {-# INLINE (<*>) #-}
  (<*>) = ap

{-# INLINE bound #-}
bound :: f (Freer f) e -> (e -> Freer f a) -> Freer f a
bound fe mkFreerA = Freer $ \_onPure onBind -> onBind fe mkFreerA

instance Monad (Freer f) where
  {-# INLINE (>>=) #-}
  (>>=) :: forall a b. Freer f a -> (a -> Freer f b) -> Freer f b
  Freer runFreerA >>= mkFreerB = runFreerA mkFreerB onBind
   where
    onBind :: forall e. f (Freer f) e -> (e -> Freer f a) -> Freer f b
    onBind fe mkFreerA = bound fe $ \e -> do
      a <- mkFreerA e
      mkFreerB a

{-# INLINE freer #-}
freer :: f (Freer f) a -> Freer f a
freer fa = bound fa pure

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
  runIt interpret =
    let go (Freer runFreer) =
          runFreer
            pure
            ( \fe mkFreer -> do
                e <- interpret fe
                go (mkFreer e)
            )
     in go

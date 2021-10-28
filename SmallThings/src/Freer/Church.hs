{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Freer.Church where

import Control.Applicative (liftA)
import Control.Monad (ap)
import Data.Kind (Type)
import Freer.Common (Interpret, RunCont)

type Freer :: ((Type -> Type) -> Type -> Type) -> (Type -> Type)
newtype Freer f a where
  Freer :: (forall r. (a -> r) -> (forall e. f (Freer f) e -> (e -> Freer f a) -> r) -> r) -> Freer f a

instance Functor (Freer f) where
  fmap = liftA

instance Applicative (Freer f) where
  pure a = Freer $ \onPure _onBind -> onPure a
  (<*>) = ap

bound :: f (Freer f) e -> (e -> Freer f a) -> Freer f a
bound fe mkFreerA = Freer $ \_onPure onBind -> onBind fe mkFreerA

instance Monad (Freer f) where
  (>>=) :: forall a b. Freer f a -> (a -> Freer f b) -> Freer f b
  Freer runFreerA >>= mkFreerB =
    runFreerA
      mkFreerB
      ( \fe mkFreerA -> bound fe $ \e -> do
          a <- mkFreerA e
          mkFreerB a
      )

freer :: f (Freer f) a -> Freer f a
freer fa = bound fa pure

run ::
  Monad m =>
  (RunCont (Freer f) f -> Interpret (Freer f) f m) ->
  Freer f a ->
  m a
run mkInterpret = runIt (mkInterpret runIt)
 where
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

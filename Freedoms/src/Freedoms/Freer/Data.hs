{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Freedoms.Freer.Data (Freer, freer, run) where

import Control.Applicative (liftA)
import Control.Monad (ap)
import Data.Kind (Type)
import Freedoms.Freer.Common (Interpret, RunCont)

type Freer :: ((Type -> Type) -> Type -> Type) -> (Type -> Type)
data Freer f a where
  Pure :: a -> Freer f a
  Bind :: f (Freer f) a -> (a -> Freer f b) -> Freer f b

instance Functor (Freer f) where
  fmap = liftA

instance Applicative (Freer f) where
  pure = Pure
  (<*>) = ap

instance Monad (Freer f) where
  (Pure a) >>= mkFreerB = mkFreerB a
  (Bind fe mkFreerA) >>= mkFreerB = Bind fe $ \e -> do
    a <- mkFreerA e
    mkFreerB a

freer :: f (Freer f) a -> Freer f a
freer fa = Bind fa pure

run ::
  Monad m =>
  (RunCont (Freer f) f -> Interpret (Freer f) f m) ->
  Freer f a ->
  m a
run mkInterpret = runIt (mkInterpret runIt)
 where
  runIt :: RunCont (Freer f) f
  runIt interpret =
    let go (Pure a) = pure a
        go (Bind fe mkFreer) = do
          e <- interpret fe
          go (mkFreer e)
     in go

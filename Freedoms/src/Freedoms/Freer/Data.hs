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
  {-# INLINE fmap #-}
  fmap = liftA

instance Applicative (Freer f) where
  {-# INLINE pure #-}
  pure = Pure
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Monad (Freer f) where
  {-# INLINE (>>=) #-}
  (Pure a) >>= mkFreerB = mkFreerB a
  (Bind fe mkFreerA) >>= mkFreerB = Bind fe $ \e -> do
    a <- mkFreerA e
    mkFreerB a

{-# INLINE freer #-}
freer :: f (Freer f) a -> Freer f a
freer fa = Bind fa pure

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
    let go (Pure a) = pure a
        go (Bind fe mkFreer) = do
          e <- interpret fe
          go (mkFreer e)
     in go

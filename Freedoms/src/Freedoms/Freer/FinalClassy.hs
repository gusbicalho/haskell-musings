{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Freedoms.Freer.FinalClassy (Freer, Interpreter (interpret), freer, run) where

import Control.Applicative (liftA)
import Control.Monad (ap)
import Data.Kind (Constraint, Type)
import Freedoms.Freer.Common (Interpret)

type Freer :: ((Type -> Type) -> Type -> Type) -> (Type -> Type)
newtype Freer f a where
  Freer :: {run :: forall m. Interpreter f m => m a} -> Freer f a

type Interpreter :: ((Type -> Type) -> Type -> Type) -> (Type -> Type) -> Constraint
class Monad m => Interpreter f m where
  interpret :: Interpret (Freer f) f m

freer :: f (Freer f) a -> Freer f a
freer fa = Freer $ interpret fa

instance Functor (Freer f) where
  fmap = liftA

instance Applicative (Freer f) where
  pure a = Freer (pure a)
  (<*>) = ap

instance Monad (Freer f) where
  Freer ma >>= mkMb = Freer $ do
    a <- ma
    run $ mkMb a

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Free.FinalClassy (Free, Interpreter (..), free, run) where

import Control.Applicative (liftA)
import Control.Monad (ap)
import Control.Monad.Trans.Reader (ReaderT (ReaderT, runReaderT))
import Data.Kind (Type)

type Free :: (Type -> Type) -> (Type -> Type)
newtype Free f a where
  Free :: {run :: forall m. Interpreter f m => m a} -> Free f a

class Monad m => Interpreter f m where
  interpret :: f a -> m a

free :: f a -> Free f a
free fa = Free $ interpret fa

instance Functor (Free f) where
  fmap = liftA

instance Applicative (Free f) where
  pure a = Free (pure a)
  (<*>) = ap

instance Monad (Free f) where
  Free ma >>= mkMb = Free $ do
    a <- ma
    run $ mkMb a

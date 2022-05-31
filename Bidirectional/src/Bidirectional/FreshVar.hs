{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Bidirectional.FreshVar (
  FreshT,
  runFreshT,
  Fresh,
  runFresh,
  FreshTypeVar,
  freshVar,
) where

import Bidirectional.Language (Var (..))
import Control.Monad.Trans.Class qualified as Trans
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Trans.State.Strict qualified as StateT
import Data.Functor.Identity (Identity)
import Data.Type.Equality (type (==))

newtype FreshVal = MkFresh Word

type FreshT = StateT FreshVal

type Fresh = FreshT Identity

runFreshT :: Monad m => FreshT m a -> m a
runFreshT = flip StateT.evalStateT (MkFresh 0)

runFresh :: FreshT Identity a -> a
runFresh = flip StateT.evalState (MkFresh 0)

freshVar :: FreshTypeVar m => String -> m Var
freshVar tag = do
  MkFresh w <- takeFresh
  pure $ FreshVar tag w

class Monad m => FreshTypeVar m where
  takeFresh :: m FreshVal

-- | Lift over ExceptT
instance (FreshTypeVar m, Monad m) => FreshTypeVar (ExceptT e m) where
  takeFresh = Trans.lift takeFresh

-- | Either work or lift over StateT
instance
  (Monad m, FreshOnStateT (FreshVal == payload) payload m) =>
  FreshTypeVar (StateT payload m)
  where
  takeFresh = takeFreshOnStateT @(FreshVal == payload)

-- | Auxiliary class to dispatch over other layers of StateT
class FreshOnStateT payloadIsFresh payload m where
  takeFreshOnStateT :: StateT payload m FreshVal

instance (Monad m, payload ~ FreshVal) => FreshOnStateT 'True payload m where
  takeFreshOnStateT = do
    MkFresh w <- StateT.get
    StateT.put $ MkFresh (succ w)
    pure $ MkFresh w

instance (Monad m, FreshTypeVar m) => FreshOnStateT 'False payload m where
  takeFreshOnStateT = Trans.lift takeFresh

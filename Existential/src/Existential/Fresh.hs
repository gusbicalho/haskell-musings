{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Existential.Fresh (
  FreshT,
  runFreshT,
  Fresh,
  runFresh,
  MonadFresh,
  fresh,
) where

import Control.Monad.Trans.Class qualified as Trans
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Trans.State.Strict qualified as StateT
import Data.Functor.Identity (Identity)
import Data.Type.Equality (type (==))
import Control.Unification.IntVar (IntBindingT)

newtype FreshVal = MkFresh {getFresh :: Word}

type FreshT = StateT FreshVal

type Fresh = FreshT Identity

runFreshT :: Monad m => FreshT m a -> m a
runFreshT = flip StateT.evalStateT (MkFresh 0)

runFresh :: FreshT Identity a -> a
runFresh = flip StateT.evalState (MkFresh 0)

fresh :: MonadFresh m => m Word
fresh = getFresh <$> takeFresh

class Monad m => MonadFresh m where
  takeFresh :: m FreshVal

-- | Lift over ExceptT
instance (MonadFresh m, Monad m) => MonadFresh (ExceptT e m) where
  takeFresh = Trans.lift takeFresh

-- | Either work or lift over StateT
instance
  (Monad m, FreshOnStateT (FreshVal == payload) payload m) =>
  MonadFresh (StateT payload m)
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

instance (Monad m, MonadFresh m) => FreshOnStateT 'False payload m where
  takeFreshOnStateT = Trans.lift takeFresh

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Existential.Surface.Unification where

import Control.Monad.Trans.Class qualified as Trans
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except qualified as ExceptT
import Control.Monad.Trans.State.Strict (StateT)
import Control.Unification qualified as Uni
import Control.Unification.IntVar (IntBindingT, IntVar, evalIntBindingT)
import Control.Unification.Types (UFailure)
import Data.Foldable qualified as F
import Data.Functor.Fixedpoint (Fix (..))
import Data.Functor.Identity (Identity)
import Data.Functor.Identity qualified as Identity
import Data.Traversable qualified as T
import Existential.Surface.Language qualified as Language
import GHC.Generics (Generic1)

type Unification = IntBindingT UniMono Identity

runUnification :: Unification a -> a
runUnification = Identity.runIdentity . evalIntBindingT

data UniMono s
  = INT
  | Var Language.TypeVar
  | MonoArrow s s
  | Projection Language.Expr Language.ExistentialType
  deriving stock (Eq, Ord, Show, Generic1, Functor, Foldable, Traversable)
  deriving anyclass (Uni.Unifiable)

type UniVar = IntVar
type UnifiableMono = Uni.UTerm UniMono UniVar

monotypeToUnifiableMono :: Language.Monotype -> UnifiableMono
monotypeToUnifiableMono = Uni.unfreeze . Fix . go
 where
  go Language.INT = INT
  go (Language.Var t) = Var t
  go (Language.MonoArrow arg ret) = MonoArrow (Fix $ go arg) (Fix $ go ret)
  go (Language.Projection e exType) = Projection e exType

unify :: UnifiableMono -> UnifiableMono -> ExceptT String Unification UnifiableMono
unify a b = stringifyErrors (Uni.unify a b)

stringifyErrors ::
  Functor n =>
  ExceptT (UFailure UniMono UniVar) n b ->
  ExceptT String n b
stringifyErrors =
  ExceptT.mapExceptT
    ( fmap \case
        Left (failure :: UFailure UniMono UniVar) -> Left (show failure)
        Right result -> Right result
    )

freezeAllWithDefault :: Language.Monotype -> [UniVar] -> ExceptT String Unification [Language.Monotype]
freezeAllWithDefault defaultMono vars = do
  let defaultUni = monotypeToUnifiableMono defaultMono
  let terms = Uni.UVar <$> vars
  allFreeVars <- Trans.lift $ Uni.getFreeVarsAll terms
  F.for_ allFreeVars (unify defaultUni . Uni.UVar)
  unifiedTerms <- stringifyErrors (Uni.applyBindingsAll terms)
  T.for unifiedTerms \term -> case Uni.freeze term of
    Nothing -> ExceptT.throwE "Unbound term in unification even after defaulting (WAT)"
    Just (Fix t) -> pure (uniMonoToMonotype t)

freezeWithDefault :: Language.Monotype -> UniVar -> ExceptT String Unification Language.Monotype
freezeWithDefault defaultMono var = do
  let defaultUni = monotypeToUnifiableMono defaultMono
  let term = Uni.UVar var
  allFreeVars <- Trans.lift $ Uni.getFreeVars term
  F.for_ allFreeVars (unify defaultUni . Uni.UVar)
  unifiedTerm <- stringifyErrors (Uni.applyBindings term)
  case Uni.freeze unifiedTerm of
    Nothing -> ExceptT.throwE "Unbound term in unification even after defaulting (WAT)"
    Just (Fix t) -> pure (uniMonoToMonotype t)

uniMonoToMonotype :: UniMono (Fix UniMono) -> Language.Monotype
uniMonoToMonotype = \case
  INT -> Language.INT
  Var var -> Language.Var var
  MonoArrow (Fix arg) (Fix ret) -> Language.MonoArrow (uniMonoToMonotype arg) (uniMonoToMonotype ret)
  Projection e t -> Language.Projection e t

class Monad m => MonadUnification m where
  freshUniVar :: m UniVar

instance MonadUnification Unification where
  freshUniVar = Uni.freeVar

-- | Lift over ExceptT
instance MonadUnification m => MonadUnification (ExceptT e m) where
  freshUniVar = Trans.lift freshUniVar

-- | Lift over StateT
instance MonadUnification m => MonadUnification (StateT e m) where
  freshUniVar = Trans.lift freshUniVar

{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE NoStarIsType #-}

module Freer.Common where

import Data.Kind (Type)

-- newtype
--   Interpret
--     (free :: Type -> Type)
--     (f :: (Type -> Type) -> Type -> Type)
--     (m :: Type -> Type)
--   where
--   Interpret ::
--     { runInterpret ::
--         forall x.
--         f free x ->
--         m x
--     } ->
--     Interpret free f m

-- type RunCont :: (Type -> Type) -> ((Type -> Type) -> Type -> Type) -> Type
-- newtype RunCont free f = RunCont
--   { runCont ::
--       forall m a.
--       Monad m =>
--       Interpret free f m ->
--       free a ->
--       m a
--   }

type Interpret
  (free :: Type -> Type)
  (f :: (Type -> Type) -> Type -> Type)
  (m :: Type -> Type) =
  forall x.
  f free x ->
  m x

type RunCont' :: (Type -> Type) -> ((Type -> Type) -> Type -> Type) -> (Type -> Type) -> Type
type RunCont' free f m =
  forall a.
  Interpret free f m ->
  free a ->
  m a

type RunCont :: (Type -> Type) -> ((Type -> Type) -> Type -> Type) -> Type
type RunCont free f = forall m. Monad m => RunCont' free f m

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Objects (Object, object, instanceOf, Implements, call, ObjectImpl(..)) where

import Control.Applicative (Alternative ((<|>)))
import Data.Functor ((<&>))
import Data.Kind (Constraint, Type)
import Data.Typeable (Typeable)
import qualified Data.Typeable as Typeable

type Object :: [Type -> Type] -> Type
data Object interfaces where
  Object ::
    FindImpl (ObjectImpl interfaces objData) objData =>
    ObjectImpl interfaces objData ->
    Object interfaces

object ::
  FindImpl (ObjectImpl interfaces objData) objData =>
  ObjectImpl interfaces objData ->
  Object interfaces
object = Object

-- | Method dispatch on existential objects
class Implements i obj where
  call :: (forall objData. i objData -> objData -> a) -> obj -> a

instance
  AsInterface i (ObjectImpl is) =>
  Implements i (Object is)
  where
  call method (Object o) = method (as @i o) (getData o)
  {-# INLINE call #-}

-- | Fallible Downcast
instanceOf ::
  forall i interfaces.
  (Typeable i) =>
  Object interfaces ->
  Maybe (Object (i : interfaces))
instanceOf (Object o) =
  findImpl' @_ @_ @i o <&> \impl -> Object (Implement impl o)

class Typeable objData => FindImpl obj objData | obj -> objData where
  findImpl' ::
    forall interface.
    (Typeable interface) =>
    obj ->
    Maybe (interface objData)

instance Typeable objData => FindImpl (ObjectImpl '[] objData) objData where
  findImpl' _ = Nothing

instance
  ( FindImpl (ObjectImpl is objData) objData
  , Typeable objData
  ) =>
  FindImpl (ObjectImpl (i : is) objData) objData
  where
  findImpl' ::
    forall interface.
    Typeable interface =>
    ObjectImpl (i : is) objData ->
    Maybe (interface objData)
  findImpl' (Implement impl super) =
    Typeable.cast @(i objData) @(interface objData) impl
      <|> findImpl' @_ @_ @interface super

type ObjectImpl :: [Type -> Type] -> Type -> Type
data ObjectImpl interfaces objData where
  ObjectData :: objData -> ObjectImpl '[] objData
  Implement ::
    Typeable interface =>
    interface objData ->
    ObjectImpl interfaces objData ->
    ObjectImpl (interface ': interfaces) objData

getData :: ObjectImpl a objData -> objData
{-# INLINE getData #-}
getData (ObjectData objData) = objData
getData (Implement _ super) = getData super

-- | Interface->implementation lookup
type AsInterface :: (Type -> Type) -> (Type -> Type) -> Constraint
class AsInterface interface object where
  as :: object a -> interface a

instance
  {-# OVERLAPPABLE #-}
  AsInterface interface (ObjectImpl moreInterfaces) =>
  AsInterface interface (ObjectImpl (otherInterface : moreInterfaces))
  where
  as (Implement _ super) = as @interface super
  {-# INLINE as #-}

instance {-# OVERLAPPING #-} AsInterface interface (ObjectImpl (interface : moreInterfaces)) where
  as (Implement implementation _) = implementation
  {-# INLINE as #-}

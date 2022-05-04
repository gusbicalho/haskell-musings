{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Structural.Copointed where

import Data.Functor.Identity (Identity (Identity))
import Data.Kind (Type)
import Data.Semigroup (First, Last, Max, Min)
import Data.Tuple (Solo)
import qualified GHC.Generics as G
import Generic.Data (Generically1 (Generically1))

class Copointed f where
  copoint :: f a -> a

deriving via (Generically1 Identity) instance (Copointed Identity)
deriving via (Generically1 Solo) instance (Copointed Solo)
deriving via (Generically1 First) instance (Copointed First)
deriving via (Generically1 Last) instance (Copointed Last)
deriving via (Generically1 Max) instance (Copointed Max)
deriving via (Generically1 Min) instance (Copointed Min)

-- Generic instance

instance (G.Generic1 f, GCopointed (G.Rep1 f)) => Copointed (Generically1 f) where
  copoint (Generically1 a) = gCopoint (G.from1 a)

class GCopointed (t :: Type -> Type) where
  gCopoint :: t x -> x

instance
  GCopointed constructors =>
  GCopointed (G.D1 meta constructors)
  where
  {-# INLINE gCopoint #-}
  gCopoint (G.M1 t) = gCopoint t

-- | This instance will only work if, at the leaves, all constructors are
-- single-fielded with a Par1
instance
  ( GCopointed left
  , GCopointed right
  ) =>
  GCopointed (left G.:+: right)
  where
  {-# INLINE gCopoint #-}
  gCopoint (G.L1 left) = gCopoint left
  gCopoint (G.R1 right) = gCopoint right

instance
  (selectors ~ G.S1 selMeta G.Par1) =>
  GCopointed (G.C1 meta selectors)
  where
  {-# INLINE gCopoint #-}
  gCopoint (G.M1 (G.M1 (G.Par1 t))) = t

-- Could support products as long as all K1 are Monoid

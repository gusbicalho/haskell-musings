{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Structural.Structure where

import Data.Data (Proxy (Proxy))
import Data.Functor.Identity (Identity (Identity))
import Data.Kind (Type)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Structural.Description (
  Description (..),
  RecordDescription (..),
  VariantDescription (..),
 )
import Structural.Utils (TyIf (..))

type Wrapper = Type -> Type

type Structure :: Wrapper -> Description Type -> Type
data Structure f structure where
  Single :: f t -> Structure f ( 'DescSingle t)
  Record :: !(RecordS f structure) -> Structure f ( 'DescRecord structure)
  Variant :: !(VariantS f structure) -> Structure f ( 'DescVariant structure)

pattern SingleI :: t -> Structure Identity ( 'DescSingle t)
pattern SingleI t = Single (Identity t)

type RecordS :: Wrapper -> RecordDescription Type -> Type
data RecordS f recordStructure where
  RecordNil :: RecordS f ( 'RecordD '[] '[])
  RecordCons ::
    forall s positional named f.
    !(Structure f s) ->
    !(RecordS f ( 'RecordD positional named)) ->
    RecordS f ( 'RecordD (s : positional) named)
  RecordConsNamed ::
    forall name s named f.
    KnownSymbol name =>
    Structure f s ->
    RecordS f ( 'RecordD '[] named) ->
    RecordS f ( 'RecordD '[] ('(name, s) : named))

type VariantS :: Wrapper -> VariantDescription Type -> Type
data VariantS f structure where
  VariantHere ::
    forall name s others f.
    !(Structure f s) ->
    VariantS f ( 'VariantD ('(name, s) : others))
  VariantThere ::
    forall name s others f.
    !(VariantS f ( 'VariantD others)) ->
    VariantS f ( 'VariantD ('(name, s) : others))

---- Eq -------------------------------
-- Eq Structure
instance Eq (f t) => Eq (Structure f ( 'DescSingle t)) where
  Single a == Single b = a == b

instance Eq (RecordS f structure) => Eq (Structure f ( 'DescRecord structure)) where
  Record a == Record b = a == b

instance Eq (VariantS f structure) => Eq (Structure f ( 'DescVariant structure)) where
  Variant a == Variant b = a == b

-- Eq Variant

instance Eq (VariantS f ( 'VariantD '[])) where
  _ == _ = error "Impossible empty variant"

instance
  ( KnownSymbol name
  , Eq (Structure f here)
  , Eq (VariantS f ( 'VariantD there))
  ) =>
  Eq (VariantS f ( 'VariantD ('(name, here) ': there)))
  where
  VariantHere a == VariantHere b = a == b
  _ == _ = False

-- Eq Record

instance Eq (RecordS f ( 'RecordD '[] '[])) where
  _ == _ = True

instance
  ( Eq (Structure f s)
  , Eq (RecordS f (RecordD '[] moreNameds))
  ) =>
  Eq (RecordS f ( 'RecordD '[] ('(name, s) : moreNameds)))
  where
  RecordConsNamed a moreA == RecordConsNamed b moreB =
    a == b && moreA == moreB

instance
  ( Eq (Structure f p)
  , Eq (RecordS f (RecordD morePs nameds))
  ) =>
  Eq (RecordS f ( 'RecordD (p : morePs) nameds))
  where
  RecordCons a moreA == RecordCons b moreB =
    a == b && moreA == moreB

---- Ord -------------------------------
-- Ord Structure
instance Ord (f t) => Ord (Structure f ( 'DescSingle t)) where
  compare (Single a) (Single b) = compare a b

instance Ord (RecordS f structure) => Ord (Structure f ( 'DescRecord structure)) where
  compare (Record a) (Record b) = compare a b

instance Ord (VariantS f structure) => Ord (Structure f ( 'DescVariant structure)) where
  compare (Variant a) (Variant b) = compare a b

-- Ord Variant

instance Ord (VariantS f ( 'VariantD '[])) where
  compare _ _ = error "Impossible empty variant"

instance
  ( KnownSymbol name
  , Ord (Structure f here)
  , Ord (VariantS f ( 'VariantD there))
  ) =>
  Ord (VariantS f ( 'VariantD ('(name, here) ': there)))
  where
  compare (VariantHere _) (VariantThere _) = LT
  compare (VariantThere _) (VariantHere _) = GT
  compare (VariantHere a) (VariantHere b) = compare a b
  compare (VariantThere a) (VariantThere b) = compare a b

-- Ord Record

instance Ord (RecordS f ( 'RecordD '[] '[])) where
  compare _ _ = EQ

instance
  ( Ord (Structure f s)
  , Ord (RecordS f (RecordD '[] moreNameds))
  ) =>
  Ord (RecordS f ( 'RecordD '[] ('(name, s) : moreNameds)))
  where
  compare (RecordConsNamed a moreA) (RecordConsNamed b moreB) =
    compare (a, moreA) (b, moreB)

instance
  ( Ord (Structure f p)
  , Ord (RecordS f (RecordD morePs nameds))
  ) =>
  Ord (RecordS f ( 'RecordD (p : morePs) nameds))
  where
  compare (RecordCons a moreA) (RecordCons b moreB) =
    compare (a, moreA) (b, moreB)

---- Show -----------------------------
-- Show Structure
instance Show (f t) => Show (Structure f ( 'DescSingle t)) where
  show (Single t) = "S (" <> show t <> ")"

instance Show (RecordS f structure) => Show (Structure f ( 'DescRecord structure)) where
  show (Record t) = "{ " <> show t <> " }"

instance Show (VariantS f structure) => Show (Structure f ( 'DescVariant structure)) where
  show (Variant t) = "<< " <> show t <> " >>"

-- Show Variant

instance Show (VariantS f ( 'VariantD '[])) where
  show _ = error "Impossible empty variant"

instance
  ( KnownSymbol name
  , Show (Structure f here)
  , Show (VariantS f ( 'VariantD there))
  ) =>
  Show (VariantS f ( 'VariantD ('(name, here) ': there)))
  where
  show (VariantHere s) = symbolVal (Proxy @name) <> ": " <> show s
  show (VariantThere s) = show s

-- Show Record

instance
  (ShowRecordS 'False recordDescription f) =>
  Show (RecordS f recordDescription)
  where
  show r = showRecord @False r

class ShowRecordS (alreadyShownSomething :: Bool) recordDescription f where
  showRecord :: RecordS f recordDescription -> String

instance ShowRecordS alreadyShownSomething ( 'RecordD '[] '[]) f where
  showRecord _ = ""

instance
  ( Show (Structure f t)
  , KnownSymbol name
  , TyIf alreadyShownSomething
  , ShowRecordS 'True ( 'RecordD '[] moreNameds) f
  ) =>
  ShowRecordS alreadyShownSomething ( 'RecordD '[] ('(name, t) : moreNameds)) f
  where
  showRecord (RecordConsNamed s more) =
    tyIf @alreadyShownSomething ", " ""
      <> symbolVal (Proxy @name)
      <> ": "
      <> show s
      <> showRecord @True more

instance
  ( Show (Structure f p)
  , TyIf alreadyShownSomething
  , ShowRecordS 'True ( 'RecordD morePs nameds) f
  ) =>
  ShowRecordS alreadyShownSomething ( 'RecordD (p : morePs) nameds) f
  where
  showRecord (RecordCons s more) =
    tyIf @alreadyShownSomething ", " ""
      <> show s
      <> showRecord @True more

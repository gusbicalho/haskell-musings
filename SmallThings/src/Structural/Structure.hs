{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE TypeFamilies #-}

module Structural.Structure where

import Data.Data (Proxy (Proxy))
import Data.Kind (Type)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Structural.Description (
  Description (..),
  RecordDescription (..),
  VariantDescription (..),
 )
import Structural.Utils (TyIf (..))

type Structure :: Description Type -> Type
data Structure structure where
  Single :: t -> Structure ( 'DescSingle t)
  Record :: !(RecordS structure) -> Structure ( 'DescRecord structure)
  Variant :: !(VariantS structure) -> Structure ( 'DescVariant structure)

type RecordS :: RecordDescription Type -> Type
data RecordS recordStructure where
  RecordNil :: RecordS ( 'RecordD '[] '[])
  RecordCons ::
    forall s positional named.
    !(Structure s) ->
    !(RecordS ( 'RecordD positional named)) ->
    RecordS ( 'RecordD (s : positional) named)
  RecordConsNamed ::
    forall name s named.
    KnownSymbol name =>
    Structure s ->
    RecordS ( 'RecordD '[] named) ->
    RecordS ( 'RecordD '[] ('(name, s) : named))

type VariantS :: VariantDescription Type -> Type
data VariantS structure where
  VariantHere ::
    forall name s others.
    !(Structure s) ->
    VariantS ( 'VariantD ('(name, s) : others))
  VariantThere ::
    forall name s others.
    !(VariantS ( 'VariantD others)) ->
    VariantS ( 'VariantD ('(name, s) : others))

---- Eq -------------------------------
-- Eq Structure
instance Eq t => Eq (Structure ( 'DescSingle t)) where
  Single a == Single b = a == b

instance Eq (RecordS structure) => Eq (Structure ( 'DescRecord structure)) where
  Record a == Record b = a == b

instance Eq (VariantS structure) => Eq (Structure ( 'DescVariant structure)) where
  Variant a == Variant b = a == b

-- Eq Variant

instance Eq (VariantS ( 'VariantD '[])) where
  _ == _ = error "Impossible empty variant"

instance
  ( KnownSymbol name
  , Eq (Structure here)
  , Eq (VariantS ( 'VariantD there))
  ) =>
  Eq (VariantS ( 'VariantD ('(name, here) ': there)))
  where
  VariantHere a == VariantHere b = a == b
  _ == _ = False

-- Eq Record

instance Eq (RecordS ( 'RecordD '[] '[])) where
  _ == _ = True

instance
  ( Eq (Structure s)
  , Eq (RecordS (RecordD '[] moreNameds))
  ) =>
  Eq (RecordS ( 'RecordD '[] ('(name, s) : moreNameds)))
  where
  RecordConsNamed a moreA == RecordConsNamed b moreB =
    a == b && moreA == moreB

instance
  ( Eq (Structure p)
  , Eq (RecordS (RecordD morePs nameds))
  ) =>
  Eq (RecordS ( 'RecordD (p : morePs) nameds))
  where
  RecordCons a moreA == RecordCons b moreB =
    a == b && moreA == moreB

---- Ord -------------------------------
-- Ord Structure
instance Ord t => Ord (Structure ( 'DescSingle t)) where
  compare (Single a) (Single b) = compare a b

instance Ord (RecordS structure) => Ord (Structure ( 'DescRecord structure)) where
  compare (Record a) (Record b) = compare a b

instance Ord (VariantS structure) => Ord (Structure ( 'DescVariant structure)) where
  compare (Variant a) (Variant b) = compare a b

-- Ord Variant

instance Ord (VariantS ( 'VariantD '[])) where
  compare _ _ = error "Impossible empty variant"

instance
  ( KnownSymbol name
  , Ord (Structure here)
  , Ord (VariantS ( 'VariantD there))
  ) =>
  Ord (VariantS ( 'VariantD ('(name, here) ': there)))
  where
  compare (VariantHere _) (VariantThere _) = LT
  compare (VariantThere _) (VariantHere _) = GT
  compare (VariantHere a) (VariantHere b) = compare a b
  compare (VariantThere a) (VariantThere b) = compare a b

-- Ord Record

instance Ord (RecordS ( 'RecordD '[] '[])) where
  compare _ _ = EQ

instance
  ( Ord (Structure s)
  , Ord (RecordS (RecordD '[] moreNameds))
  ) =>
  Ord (RecordS ( 'RecordD '[] ('(name, s) : moreNameds)))
  where
  compare (RecordConsNamed a moreA) (RecordConsNamed b moreB) =
    compare (a, moreA) (b, moreB)

instance
  ( Ord (Structure p)
  , Ord (RecordS (RecordD morePs nameds))
  ) =>
  Ord (RecordS ( 'RecordD (p : morePs) nameds))
  where
  compare (RecordCons a moreA) (RecordCons b moreB) =
    compare (a, moreA) (b, moreB)

---- Show -----------------------------
-- Show Structure
instance Show t => Show (Structure ( 'DescSingle t)) where
  show (Single t) = "S (" <> show t <> ")"

instance Show (RecordS structure) => Show (Structure ( 'DescRecord structure)) where
  show (Record t) = "{ " <> show t <> " }"

instance Show (VariantS structure) => Show (Structure ( 'DescVariant structure)) where
  show (Variant t) = "<< " <> show t <> " >>"

-- Show Variant

instance Show (VariantS ( 'VariantD '[])) where
  show _ = error "Impossible empty variant"

instance
  ( KnownSymbol name
  , Show (Structure here)
  , Show (VariantS ( 'VariantD there))
  ) =>
  Show (VariantS ( 'VariantD ('(name, here) ': there)))
  where
  show (VariantHere s) = symbolVal (Proxy @name) <> ": " <> show s
  show (VariantThere s) = show s

-- Show Record

instance
  (ShowRecordS 'False recordDescription) =>
  Show (RecordS recordDescription)
  where
  show r = showRecord @False r

class ShowRecordS (alreadyShownSomething :: Bool) recordDescription where
  showRecord :: RecordS recordDescription -> String

instance ShowRecordS alreadyShownSomething ( 'RecordD '[] '[]) where
  showRecord _ = ""

instance
  ( Show (Structure t)
  , KnownSymbol name
  , TyIf alreadyShownSomething
  , ShowRecordS 'True ( 'RecordD '[] moreNameds)
  ) =>
  ShowRecordS alreadyShownSomething ( 'RecordD '[] ('(name, t) : moreNameds))
  where
  showRecord (RecordConsNamed s more) =
    tyIf @alreadyShownSomething ", " ""
      <> symbolVal (Proxy @name)
      <> ": "
      <> show s
      <> showRecord @True more

instance
  ( Show (Structure p)
  , TyIf alreadyShownSomething
  , ShowRecordS 'True ( 'RecordD morePs nameds)
  ) =>
  ShowRecordS alreadyShownSomething ( 'RecordD (p : morePs) nameds)
  where
  showRecord (RecordCons s more) =
    tyIf @alreadyShownSomething ", " ""
      <> show s
      <> showRecord @True more

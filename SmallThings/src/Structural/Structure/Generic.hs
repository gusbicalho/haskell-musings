{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Structural.Structure.Generic (GDatatypeAsStructure (..)) where

import Data.Kind (Type)
import qualified GHC.Generics as G
import GHC.TypeLits (KnownSymbol)
import Structural.Description (
  AddVariantMember,
  ConcatRecordDescriptions,
  Description (..),
  RecordDescription (..),
  VariantDescription (..),
 )
import Structural.Structure (
  RecordS (..),
  Structure (..),
  VariantS (..),
 )
import Structural.Structure.Records (
  ConcatRecords (..),
  SplitRecord (..),
 )
import Structural.Utils (ConstructorName)

-- | Representation of a datatype as a Structure
class GDatatypeAsStructure (t :: k -> Type) where
  type GDatatypeDescription t :: Description Type
  gToStructure :: t x -> Structure (GDatatypeDescription t)
  gFromStructure :: Structure (GDatatypeDescription t) -> t x

-- | Representation of a single constructor as a Structure
class GConstructorAsStructure (constructor :: k -> Type) where
  type GConstructorDescription constructor :: Description Type
  gConstructorToStructure :: constructor x -> Structure (GConstructorDescription constructor)
  gConstructorFromStructure :: Structure (GConstructorDescription constructor) -> constructor x

-- | Representation of a sum of multiple constructors as a VariantS
class GSumAsVariant (left :: k -> Type) (right :: k -> Type) where
  type GSumDescription left right :: VariantDescription Type
  gSumToVariant :: (left G.:+: right) x -> VariantS (GSumDescription left right)
  gSumFromVariant :: VariantS (GSumDescription left right) -> (left G.:+: right) x

-- | Representation of multiple field selectors as a RecordS
class GFieldSelectorsAsRecord (t :: k -> Type) where
  type GFieldSelectorsDescription t :: RecordDescription Type
  gFieldsToRecord :: t x -> RecordS (GFieldSelectorsDescription t)
  gFieldsFromRecord :: RecordS (GFieldSelectorsDescription t) -> t x

-- GDatatypeAsStructure instances

-- A datatype can be represented if its constructors can be represented
instance GDatatypeAsStructure constructors => GDatatypeAsStructure (G.D1 meta constructors) where
  type GDatatypeDescription (G.D1 meta constructors) = GDatatypeDescription constructors
  gToStructure (G.M1 t) = gToStructure t
  gFromStructure s = G.M1 (gFromStructure s)

-- A datatype with multiple constructors can be represented as a Variant
instance
  GSumAsVariant left right =>
  GDatatypeAsStructure (left G.:+: right)
  where
  type
    GDatatypeDescription (left G.:+: right) =
      'DescVariant (GSumDescription left right)
  gToStructure t = Variant $ gSumToVariant @_ @left @right t
  gFromStructure (Variant s) = gSumFromVariant @_ @left @right s

-- A datatype with a single constructor can be represented if the constructor can be represented
instance GConstructorAsStructure constructor => GDatatypeAsStructure (G.C1 meta constructor) where
  type GDatatypeDescription (G.C1 meta constructor) = GConstructorDescription constructor
  gToStructure (G.M1 t) = gConstructorToStructure t
  gFromStructure s = G.M1 (gConstructorFromStructure s)

-- GConstructorAsStructure instances

-- A single nullary constructor can be represented as Single ()
instance GConstructorAsStructure (G.C1 meta G.U1) where
  type GConstructorDescription (G.C1 meta G.U1) = 'DescSingle ()
  gConstructorToStructure (G.M1 _) = Single ()
  gConstructorFromStructure _ = G.M1 G.U1

-- A constructor with a single unnamed field can be represented as a single
instance GConstructorAsStructure (G.C1 meta (G.S1 ( 'G.MetaSel 'Nothing su ss ds) (G.K1 i t))) where
  type
    GConstructorDescription (G.C1 meta (G.S1 ( 'G.MetaSel 'Nothing su ss ds) (G.K1 i t))) =
      'DescSingle t
  gConstructorToStructure (G.M1 (G.M1 (G.K1 t))) = Single t
  gConstructorFromStructure (Single t) = (G.M1 (G.M1 (G.K1 t)))

-- A constructor with a single named field can be represented as a variant
instance GConstructorAsStructure (G.C1 meta (G.S1 ( 'G.MetaSel ( 'Just fieldName) su ss ds) (G.K1 i t))) where
  type
    GConstructorDescription (G.C1 meta (G.S1 ( 'G.MetaSel ( 'Just fieldName) su ss ds) (G.K1 i t))) =
      'DescVariant ( 'VariantD '[ '(fieldName, 'DescSingle t)])
  gConstructorToStructure (G.M1 (G.M1 (G.K1 t))) = Variant $ VariantHere (Single t)
  gConstructorFromStructure (Variant (VariantHere (Single t))) = G.M1 (G.M1 (G.K1 t))

-- A constructor with multiple fields can be represented as a record
instance
  GFieldSelectorsAsRecord (left G.:*: right) =>
  GConstructorAsStructure (G.C1 meta (left G.:*: right))
  where
  type
    GConstructorDescription (G.C1 meta (left G.:*: right)) =
      'DescRecord (GFieldSelectorsDescription (left G.:*: right))
  gConstructorToStructure (G.M1 t) = Record $ gFieldsToRecord t
  gConstructorFromStructure (Record s) = G.M1 (gFieldsFromRecord s)

-- GFieldSelectorsAsRecord instances

-- A pair of selectors is the concatenation of two record
instance
  ( GFieldSelectorsAsRecord f
  , GFieldSelectorsAsRecord g
  , ConcatRecords
      (GFieldSelectorsDescription f)
      (GFieldSelectorsDescription g)
  , SplitRecord (ConcatRecordDescriptions (GFieldSelectorsDescription f) (GFieldSelectorsDescription g)) (GFieldSelectorsDescription f)
  , GFieldSelectorsDescription g
      ~ SplitLeftover (ConcatRecordDescriptions (GFieldSelectorsDescription f) (GFieldSelectorsDescription g)) (GFieldSelectorsDescription f)
  ) =>
  GFieldSelectorsAsRecord (f G.:*: g)
  where
  type
    GFieldSelectorsDescription (f G.:*: g) =
      ConcatRecordDescriptions (GFieldSelectorsDescription f) (GFieldSelectorsDescription g)
  gFieldsToRecord (f G.:*: g) = concatRecords (gFieldsToRecord f) (gFieldsToRecord g)
  gFieldsFromRecord record =
    let (left, right) = splitRecord @_ @(GFieldSelectorsDescription f) record
     in gFieldsFromRecord left G.:*: gFieldsFromRecord right

-- A single named selector is a single named record field
instance
  KnownSymbol fieldName =>
  GFieldSelectorsAsRecord (G.S1 ( 'G.MetaSel ( 'Just fieldName) su ss ds) (G.K1 i t))
  where
  type
    GFieldSelectorsDescription (G.S1 ( 'G.MetaSel ( 'Just fieldName) su ss ds) (G.K1 i t)) =
      'RecordD '[] '[ '(fieldName, 'DescSingle t)]
  gFieldsToRecord (G.M1 (G.K1 v)) = RecordConsNamed @fieldName (Single v) RecordNil
  gFieldsFromRecord (RecordConsNamed (Single v) RecordNil) = G.M1 (G.K1 v)

-- A single unnamed selector is a single positional record field
instance GFieldSelectorsAsRecord (G.S1 ( 'G.MetaSel 'Nothing su ss ds) (G.K1 i t)) where
  type
    GFieldSelectorsDescription (G.S1 ( 'G.MetaSel 'Nothing su ss ds) (G.K1 i t)) =
      'RecordD '[ 'DescSingle t] '[]
  gFieldsToRecord (G.M1 (G.K1 v)) = RecordCons (Single v) RecordNil
  gFieldsFromRecord (RecordCons (Single v) RecordNil) = G.M1 (G.K1 v)

-- GSumAsVariant instances

-- If we have multiple constructors on the left, we navigate leftwards on the tree
instance
  GSumAsVariant left (middle G.:+: right) =>
  GSumAsVariant (left G.:+: middle) right
  where
  type
    GSumDescription (left G.:+: middle) right =
      GSumDescription left (middle G.:+: right)
  gSumToVariant = \case
    G.L1 (G.L1 left) -> gSumToVariant @_ @left @(middle G.:+: right) (G.L1 left)
    G.L1 (G.R1 middle) -> gSumToVariant @_ @left @(middle G.:+: right) (G.R1 (G.L1 middle))
    G.R1 right -> gSumToVariant @_ @left @(middle G.:+: right) (G.R1 (G.R1 right))
  gSumFromVariant v =
    case gSumFromVariant @_ @left @(middle G.:+: right) v of
      (G.L1 left) -> G.L1 (G.L1 left)
      G.R1 (G.L1 middle) -> G.L1 (G.R1 middle)
      G.R1 (G.R1 right) -> G.R1 right

-- When we reach the leftmost constructor, we turn it into the first Variant case
-- and then we work our way to the right to build the other cases
instance
  ( GConstructorAsStructure (G.C1 meta selectors)
  , hereName ~ ConstructorName meta
  , hereStructure ~ GConstructorDescription (G.C1 meta selectors)
  , 'VariantD restVariant ~ GSumDescription middle right
  , 'VariantD ('(hereName, hereStructure) : restVariant)
      ~ GSumDescription (G.C1 meta selectors) (middle G.:+: right)
  , GSumAsVariant middle right
  ) =>
  GSumAsVariant (G.C1 meta selectors) (middle G.:+: right)
  where
  type
    GSumDescription (G.C1 meta selectors) (middle G.:+: right) =
      AddVariantMember
        (ConstructorName meta)
        (GConstructorDescription (G.C1 meta selectors))
        (GSumDescription middle right)
  gSumToVariant = \case
    G.L1 here -> VariantHere $ gConstructorToStructure here
    G.R1 there -> VariantThere $ gSumToVariant there
  gSumFromVariant = \case
    VariantHere here -> G.L1 (gConstructorFromStructure here)
    VariantThere there -> G.R1 (gSumFromVariant there)

-- When we only have two constructors left, we reached the base case
-- We represent this as a Variant with exactly two cases
instance
  ( GConstructorAsStructure (G.C1 metaHere selectorsHere)
  , GConstructorAsStructure (G.C1 metaThere selectorsThere)
  ) =>
  GSumAsVariant (G.C1 metaHere selectorsHere) (G.C1 metaThere selectorsThere)
  where
  type
    GSumDescription (G.C1 metaHere selectorsHere) (G.C1 metaThere selectorsThere) =
      'VariantD
        '[ '(ConstructorName metaHere, GConstructorDescription (G.C1 metaHere selectorsHere))
         , '(ConstructorName metaThere, GConstructorDescription (G.C1 metaThere selectorsThere))
         ]
  gSumToVariant = \case
    G.L1 here -> VariantHere $ gConstructorToStructure here
    G.R1 there -> VariantThere . VariantHere $ gConstructorToStructure there
  gSumFromVariant = \case
    VariantHere here -> G.L1 (gConstructorFromStructure here)
    VariantThere (VariantHere there) -> G.R1 (gConstructorFromStructure there)

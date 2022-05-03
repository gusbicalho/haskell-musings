{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Structural.Description where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)

data Description k where
  DescSingle :: k -> Description k
  DescRecord :: RecordDescription k -> Description k
  DescVariant :: VariantDescription k -> Description k

data RecordDescription k where
  RecordD ::
    [Description k] ->
    [(Symbol, Description k)] ->
    RecordDescription k

data VariantDescription k where
  VariantD :: [(Symbol, Description k)] -> VariantDescription k

type Desc :: Description k -> Type
data Desc description = Desc

type Rec posFields namedFields = 'DescRecord ( 'RecordD posFields namedFields)
type Var variants = 'DescVariant ( 'VariantD variants)
type Unit = 'DescSingle ()

-- Helpful type families

type ConcatRecordDescriptions :: RecordDescription k -> RecordDescription k -> RecordDescription k
type family ConcatRecordDescriptions r1 r2 where
  ConcatRecordDescriptions ( 'RecordD positionals nameds) r2 =
    AddNamedRecordFields nameds (AddPositionalRecordFields positionals r2)

type AddNamedRecordFields :: [(Symbol, Description k)] -> RecordDescription k -> RecordDescription k
type family AddNamedRecordFields nameds r where
  AddNamedRecordFields '[] r = r
  AddNamedRecordFields ('(s, k) : nameds) r =
    AddNamedRecordField s k (AddNamedRecordFields nameds r)

type AddNamedRecordField :: Symbol -> Description k -> RecordDescription k -> RecordDescription k
type family AddNamedRecordField s k r where
  AddNamedRecordField s k ( 'RecordD positionals nameds) = 'RecordD positionals ('(s, k) : nameds)

type AddPositionalRecordFields :: [Description k] -> RecordDescription k -> RecordDescription k
type family AddPositionalRecordFields nameds r where
  AddPositionalRecordFields '[] r = r
  AddPositionalRecordFields (k : positionals) r =
    AddPositionalRecordField k (AddPositionalRecordFields positionals r)

type AddPositionalRecordField :: Description k -> RecordDescription k -> RecordDescription k
type family AddPositionalRecordField k r where
  AddPositionalRecordField k ( 'RecordD positionals nameds) = 'RecordD (k : positionals) nameds

type ConcatVariantDescriptions :: VariantDescription k -> VariantDescription k -> VariantDescription k
type family ConcatVariantDescriptions v1 v2 where
  ConcatVariantDescriptions ( 'VariantD '[]) v2 = v2
  ConcatVariantDescriptions ( 'VariantD ('(s, k) : more)) v2 =
    AddVariantMember s k (ConcatVariantDescriptions ( 'VariantD more) v2)

type AddVariantMember :: Symbol -> Description k -> VariantDescription k -> VariantDescription k
type family AddVariantMember s k r where
  AddVariantMember s k ( 'VariantD members) = 'VariantD ('(s, k) : members)

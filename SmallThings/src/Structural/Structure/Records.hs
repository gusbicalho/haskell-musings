{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Structural.Structure.Records (
  ConsNamed,
  consNamed,
  ConcatRecords,
  concatRecords,
  SplitRecord,
  SplitLeftover,
  splitRecord,
) where

import Data.Kind (Type)
import GHC.TypeLits (KnownSymbol)
import Structural.Description (
  AddNamedRecordField,
  AddPositionalRecordField,
  ConcatRecordDescriptions,
  RecordDescription (..),
 )
import Structural.Structure (RecordS (..), Structure)
import Structural.Utils (IsSameSymbol)

-- | Cons a named field to a Record
class KnownSymbol name => ConsNamed r name s where
  consNamed' :: Structure f s -> RecordS f r -> RecordS f (AddNamedRecordField name s r)

{-# INLINE consNamed #-}
consNamed :: forall name s r f. ConsNamed r name s => Structure f s -> RecordS f r -> RecordS f (AddNamedRecordField name s r)
consNamed = consNamed' @r @name

instance KnownSymbol name => ConsNamed ( 'RecordD '[] nameds) name s where
  {-# INLINE consNamed' #-}
  consNamed' s r = RecordConsNamed @name s r

instance (ConsNamed ( 'RecordD morePs nameds) name s) => ConsNamed ( 'RecordD (p ': morePs) nameds) name s where
  {-# INLINE consNamed' #-}
  consNamed' s (RecordCons p r) = RecordCons p $ consNamed @name @s @(( 'RecordD morePs nameds)) s r

-- | Concatenate two records by concatenating their positional and named fields
class ConcatRecords r1 r2 where
  concatRecords :: RecordS f r1 -> RecordS f r2 -> RecordS f (ConcatRecordDescriptions r1 r2)

instance ConcatRecords ( 'RecordD '[] '[]) r2 where
  {-# INLINE concatRecords #-}
  concatRecords _ r2 = r2

instance
  ( ConcatRecords ( 'RecordD '[] nameds) r2
  , 'RecordD allPositionals moreNameds
      ~ ConcatRecordDescriptions ( 'RecordD '[] nameds) r2
  , ConsNamed ( 'RecordD allPositionals moreNameds) name t
  ) =>
  ConcatRecords ( 'RecordD '[] ('(name, t) : nameds)) r2
  where
  {-# INLINE concatRecords #-}
  concatRecords (RecordConsNamed v more) r2 =
    consNamed @name @t @( 'RecordD allPositionals moreNameds) v (concatRecords more r2)

instance
  ( ConcatRecords ( 'RecordD positionals allNameds) r2
  , 'RecordD morePositionals allNameds
      ~ ConcatRecordDescriptions ( 'RecordD positionals allNameds) r2
  , 'RecordD (t : morePositionals) allNameds
      ~ ConcatRecordDescriptions ( 'RecordD (t : positionals) allNameds) r2
  ) =>
  ConcatRecords ( 'RecordD (t : positionals) allNameds) r2
  where
  {-# INLINE concatRecords #-}
  concatRecords (RecordCons v more) r2 =
    RecordCons @t @morePositionals @allNameds v (concatRecords more r2)

-- | Extracts part of a record into a separate record, returning that along with leftovers from the original
class SplitRecord fullRecord toExtract where
  type SplitLeftover fullRecord toExtract :: RecordDescription Type
  splitRecord :: RecordS f fullRecord -> (RecordS f toExtract, RecordS f (SplitLeftover fullRecord toExtract))

instance (toExtract ~ 'RecordD '[] '[]) => SplitRecord ( 'RecordD '[] '[]) toExtract where
  type SplitLeftover ( 'RecordD '[] '[]) toExtract = ( 'RecordD '[] '[])
  {-# INLINE splitRecord #-}
  splitRecord full = (RecordNil, full)

instance
  ( full_p ~ extr_p
  , SplitRecord
      ( 'RecordD full_morePs full_nameds)
      ( 'RecordD extr_morePs extr_nameds)
  ) =>
  SplitRecord ( 'RecordD (full_p ': full_morePs) full_nameds) ( 'RecordD (extr_p ': extr_morePs) extr_nameds)
  where
  type
    SplitLeftover ( 'RecordD (full_p ': full_morePs) full_nameds) ( 'RecordD (extr_p ': extr_morePs) extr_nameds) =
      SplitLeftover ( 'RecordD (full_morePs) full_nameds) ( 'RecordD (extr_morePs) extr_nameds)
  {-# INLINE splitRecord #-}
  splitRecord (RecordCons s r) =
    let (rest, leftover) = splitRecord r
     in (RecordCons s rest, leftover)

instance
  ( SplitRecord
      ( 'RecordD full_morePs full_nameds)
      ( 'RecordD '[] extr_nameds)
  , 'RecordD full_morePs someNameds
      ~ SplitLeftover ( 'RecordD full_morePs full_nameds) ( 'RecordD '[] extr_nameds)
  ) =>
  SplitRecord ( 'RecordD (full_p ': full_morePs) full_nameds) ( 'RecordD '[] extr_nameds)
  where
  type
    SplitLeftover ( 'RecordD (full_p ': full_morePs) full_nameds) ( 'RecordD '[] extr_nameds) =
      AddPositionalRecordField
        full_p
        (SplitLeftover ( 'RecordD full_morePs full_nameds) ( 'RecordD '[] extr_nameds))
  {-# INLINE splitRecord #-}
  splitRecord (RecordCons s r) =
    let (rest, leftover) = splitRecord @( 'RecordD full_morePs full_nameds) @( 'RecordD '[] extr_nameds) r
     in (rest, RecordCons s leftover)

instance
  ( namesMatch ~ IsSameSymbol full_name extr_name
  , SplitOnName
      namesMatch
      ( 'RecordD '[] ('(full_name, full_s) : full_moreNameds))
      ( 'RecordD '[] ('(extr_name, extr_s) : extr_moreNameds))
  ) =>
  SplitRecord ( 'RecordD '[] ('(full_name, full_s) : full_moreNameds)) ( 'RecordD '[] ('(extr_name, extr_s) : extr_moreNameds))
  where
  type
    SplitLeftover ( 'RecordD '[] ('(full_name, full_s) : full_moreNameds)) ( 'RecordD '[] ('(extr_name, extr_s) : extr_moreNameds)) =
      LeftoverOnName
        (IsSameSymbol full_name extr_name)
        ( 'RecordD '[] ('(full_name, full_s) : full_moreNameds))
        ( 'RecordD '[] ('(extr_name, extr_s) : extr_moreNameds))
  {-# INLINE splitRecord #-}
  splitRecord r = splitRecordOnName @namesMatch r

instance SplitRecord ( 'RecordD '[] ('(full_name, full_s) : full_moreNameds)) ( 'RecordD '[] '[]) where
  type
    SplitLeftover ( 'RecordD '[] ('(full_name, full_s) : full_moreNameds)) ( 'RecordD '[] '[]) =
      ( 'RecordD '[] ('(full_name, full_s) : full_moreNameds))
  {-# INLINE splitRecord #-}
  splitRecord r = (RecordNil, r)

class SplitOnName (nameMatches :: Bool) fullRecord toExtract where
  type LeftoverOnName nameMatches fullRecord toExtract :: RecordDescription Type
  splitRecordOnName :: RecordS f fullRecord -> (RecordS f toExtract, RecordS f (LeftoverOnName nameMatches fullRecord toExtract))

instance
  ( full_named ~ extr_named
  , full_named ~ '(full_name, full_s)
  , extr_named ~ '(extr_name, extr_s)
  , SplitRecord
      ( 'RecordD '[] full_moreNameds)
      ( 'RecordD '[] extr_moreNameds)
  ) =>
  SplitOnName 'True ( 'RecordD '[] (full_named : full_moreNameds)) ( 'RecordD '[] (extr_named : extr_moreNameds))
  where
  type
    LeftoverOnName 'True ( 'RecordD '[] (full_named : full_moreNameds)) ( 'RecordD '[] (extr_named : extr_moreNameds)) =
      SplitLeftover ( 'RecordD '[] full_moreNameds) ( 'RecordD '[] extr_moreNameds)
  {-# INLINE splitRecordOnName #-}
  splitRecordOnName (RecordConsNamed s r) =
    let (rest, leftover) = splitRecord @( 'RecordD '[] full_moreNameds) @( 'RecordD '[] extr_moreNameds) r
     in (RecordConsNamed s rest, leftover)

instance
  ( SplitRecord
      ( 'RecordD '[] full_moreNameds)
      ( 'RecordD '[] ('(extr_name, extr_s) : extr_moreNameds))
  , 'RecordD '[] someNameds
      ~ SplitLeftover ( 'RecordD '[] full_moreNameds) ( 'RecordD '[] ('(extr_name, extr_s) : extr_moreNameds))
  ) =>
  SplitOnName 'False ( 'RecordD '[] ('(full_name, full_s) : full_moreNameds)) ( 'RecordD '[] ('(extr_name, extr_s) : extr_moreNameds))
  where
  type
    LeftoverOnName 'False ( 'RecordD '[] ('(full_name, full_s) : full_moreNameds)) ( 'RecordD '[] ('(extr_name, extr_s) : extr_moreNameds)) =
      AddNamedRecordField
        full_name
        full_s
        (SplitLeftover ( 'RecordD '[] full_moreNameds) ( 'RecordD '[] ('(extr_name, extr_s) : extr_moreNameds)))
  {-# INLINE splitRecordOnName #-}
  splitRecordOnName (RecordConsNamed s r) =
    let (rest, leftover) = splitRecord @( 'RecordD '[] full_moreNameds) @( 'RecordD '[] ('(extr_name, extr_s) : extr_moreNameds)) r
     in (rest, RecordConsNamed s leftover)

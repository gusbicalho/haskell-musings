-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module Structural where

import Data.Kind (Type)
import Data.List (unfoldr)
import qualified GHC.Generics as G
import Generic.Data (Generically (..))
import Structural.Description (Description (..), Rec, RecordDescription (..), Unit, Var, VariantDescription (..))
import Structural.Structure (RecordS (..), Structure (..), VariantS (..))
import Structural.Structure.Generic (GDatatypeAsStructure (..))

type Iso a b = (a -> b, b -> a)

class AsStructure a where
  type StructureRep a :: Description Type
  toStructure :: a -> Structure (StructureRep a)
  fromStructure :: Structure (StructureRep a) -> a

instance
  ( G.Generic a
  , GDatatypeAsStructure (G.Rep a)
  ) =>
  AsStructure (Generically a)
  where
  type StructureRep (Generically a) = GDatatypeDescription (G.Rep a)
  toStructure = gToStructure . G.from
  fromStructure = G.to . gFromStructure

instance AsStructure () where
  type StructureRep () = 'DescSingle ()
  toStructure _ = Single ()
  fromStructure _ = ()

-- Examples

a ::
  Structure
    ( 'DescVariant
        ( 'VariantD
            '[ '("[]", 'DescSingle ())
             , '( ":"
                , 'DescRecord
                    ( 'RecordD '[ 'DescSingle Char, 'DescSingle [Char]] '[])
                )
             ]
        )
    )
a = gToStructure . G.from $ ['a']

b ::
  ( StructureRep a
      ~ 'DescVariant
          ( 'VariantD
              ( '(name1, s)
                  : '( name2
                     , 'DescRecord ( 'RecordD '[ 'DescSingle Char, 'DescSingle t] '[])
                     )
                    : others1
              )
          )
  , AsStructure a
  , StructureRep t
      ~ 'DescVariant ( 'VariantD ('(name3, 'DescSingle ()) : others2))
  , AsStructure t
  ) =>
  () ->
  a
b () =
  fromStructure $ cons 'a' nil
 where
  cons x xs =
    Variant . VariantThere . VariantHere $
      (Record . RecordCons (Single x) . RecordCons xs $ RecordNil)
  nil = Single $ fromStructure $ (Variant $ VariantHere $ Single ())

data ListC = End | Cons Char ListC
  deriving (G.Generic, Show)
  deriving (AsStructure) via (Generically ListC)

deriving via (Generically String) instance (AsStructure String)

b1 :: String
b1 = b () :: String

b2 :: ListC
b2 = b () :: ListC

-- Hand-coded example

newtype StructuralList t
  = StructuralList
      ( Structure
          ( Var
              '[ '("nil", Unit)
               , '( "cons"
                  , Rec
                      '[]
                      '[ '("value", 'DescSingle t)
                       , '("next", 'DescSingle (StructuralList t))
                       ]
                  )
               ]
          )
      )
  deriving (Show)

lToSL :: [t] -> StructuralList t
lToSL = foldr cons nil
 where
  nil = StructuralList (Variant (VariantHere @"nil" (Single ())))
  cons x sl =
    StructuralList
      . Variant
      . VariantThere @"nil"
      . VariantHere @"cons"
      . Record
      . RecordConsNamed @"value" (Single x)
      . RecordConsNamed @"next" (Single sl)
      $ RecordNil

slToL :: StructuralList t -> [t]
slToL = unfoldr go
 where
  go (StructuralList (Variant sl)) = case sl of
    VariantHere _nil -> Nothing
    VariantThere (VariantHere (Record (Single val `RecordConsNamed` (Single next `RecordConsNamed` _)))) ->
      Just (val, next)

x :: StructuralList Char
x = lToSL ['a', 'b', 'c']

-- >>> x
-- StructuralList << cons: { value: S ('a'), next: S (StructuralList << cons: { value: S ('b'), next: S (StructuralList << cons: { value: S ('c'), next: S (StructuralList << nil: S (()) >>) } >>) } >>) } >>

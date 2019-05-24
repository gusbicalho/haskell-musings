{-# LANGUAGE TypeFamilies, TypeOperators, UndecidableInstances #-}

module TypeLevel.Set (
  ToSet, Add, Delete, Union, Difference
  ) where

  import TypeLevel.List (DeleteOne, InsertSorted, Nub, Sort, type (++))

  type family Add' a bs where
    Add' a bs = InsertSorted a (DeleteOne a bs)

  -- Nub only deletes sequences of duplicates
  type family Nub' as where
    Nub'    '[]       = '[]
    Nub' (x : x : xs) = Nub' (x : xs)
    Nub' (x : xs)     = x : Nub' xs

  type family Setlike as where
    Setlike as = Nub' (Sort as)

  type family Union' as bs where
    Union' as bs = Setlike (as ++ bs)

  type family Difference' as bs where
    Difference' as '[] = as
    Difference' '[] bs = '[]
    Difference' as (b ': bs) = Difference' (DeleteOne b as) bs

  -- Set kind
  data Set (as :: [*])

  type family ToSet as where
    ToSet as = Set (Setlike as)

  type family Add a bs where
    Add a (Set bs) = Set (Add' a bs)

  type family Delete a bs where
    Delete a (Set bs) = Set (DeleteOne a bs)

  type family Union as bs where
    Union (Set as) (Set bs) = Set (Union' as bs)

  type family Difference as bs where
    Difference (Set as) (Set bs) = Set (Difference' as bs)

{--
Run the >>> blocks below in GHCi to try it out

>>> :set -XDataKinds -XDeriveGeneric -XGADTs -XKindSignatures
>>> import GHC.Generics
>>> data X deriving Generic
>>> data Y deriving Generic
>>> data Z deriving Generic
>>> :kind! ToSet '[X, X, Y, Z, Y, Y, Z]
ToSet '[X, X, Y, Z, Y, Y, Z] :: *
= Set '[X, Y, Z]

>>> :kind! Union (ToSet '[Y, X, X, Y]) (ToSet '[Z])
Union (ToSet '[Y, X, X, Y]) (ToSet '[Z]) :: *
= Set '[X, Y, Z]

>>> data Bla (ts :: *) where Bla :: Integer -> Bla ts deriving (Eq)
>>> (Bla 1 :: Bla (ToSet '[X, Y, Z])) == (Bla 1 :: Bla (ToSet '[Z, X, Y]))
True

--}

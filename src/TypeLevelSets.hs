{-# LANGUAGE
    DataKinds, DeriveGeneric, FlexibleContexts, FlexibleInstances, FunctionalDependencies,
    GADTs, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, TypeOperators,
    UndecidableInstances
#-}
module TypeLevelSets (
  ToSet, Add, Delete, Union, Difference
) where

  import GHC.TypeLits
  import GHC.Generics
  import Data.Type.Bool

  -- Type -> Symbol
  -- from http://www.mchaver.com/posts/2017-12-12-type-name-to-string.html
  type family TypeName a :: Symbol where
    TypeName Double = "Double"
    TypeName Int = "Int"
    TypeName String = "String"
    TypeName (M1 D ('MetaData name _ _ _) f ()) = name
    TypeName a = TypeName (Rep a ())

  type family CmpName a b :: Ordering where
    CmpName a b = CmpSymbol (TypeName a) (TypeName b)

  -- insertion sort from https://kseo.github.io/posts/2017-01-30-type-level-insertion-sort.html
  type family Sort xs where
    Sort '[] = '[]
    Sort (x ': xs) = Insert x (Sort xs)

  type family Insert x xs where
    Insert x '[] = x ': '[]
    Insert x (y ': ys) = Insert' (CmpName x y) x y ys

  type family Insert' b x y ys where
    Insert' 'LT  x y ys = x ': (y ': ys)
    Insert' _    x y ys = y ': Insert x ys

  -- List append
  type family (++) as bs where
    '[] ++ bs = bs
    (a : as) ++ bs = a : (as ++ bs)

  -- List as set

  -- Naive Delete - assumes there is only one of each
  type family Delete' a bs where
    Delete' a '[] = '[]
    Delete' a (a ': t) = t
    Delete' a (h ': t) = h ': Delete' a t

  type family Add' a bs where
    Add' a bs = Insert a (Delete' a bs)

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
    Difference' as (b ': bs) = Difference' (Delete' b as) bs

  -- Set kind
  data Set (as :: [*])

  type family ToSet as where
    ToSet as = Set (Setlike as)

  type family Add a bs where
    Add a (Set bs) = Set (Add' a bs)

  type family Delete a bs where
    Delete a (Set bs) = Set (Delete' a bs)

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

>>> :kind! Nub' '[X, X, Y, Z, Y, Y, Z]
Nub' '[X, X, Y, Z, Y, Y, Z] :: [*]
= '[X, Y, Z, Y, Z]

>>> :kind! (Sort '[Z, Y, X])
(Sort '[Z, Y, X]) :: [*]
= '[X, Y, Z]

>>> data Bla (ts :: *) where Bla :: Integer -> Bla ts deriving (Eq)
>>> (Bla 1 :: Bla (ToSet '[X, Y, Z])) == (Bla 1 :: Bla (ToSet '[Z, X, Y]))
True

--}


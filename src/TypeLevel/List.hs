{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, TypeOperators, UndecidableInstances #-}

module TypeLevel.List (
  Cmp, CmpName, Sort, InsertSorted, type (++), Nub, Has, DeleteOne
  ) where

  import GHC.TypeLits
  import Data.Type.Bool
  import TypeLevel.Reflection (TypeName)

  type family Cmp token a b :: Ordering

  data CmpName
  type instance Cmp CmpName a b = CmpSymbol (TypeName a) (TypeName b)

  -- insertion sort from https://kseo.github.io/posts/2017-01-30-type-level-insertion-sort.html
  -- using the approach from https://stackoverflow.com/a/50494615 to parametrization
  type family SortBy cmp xs where
    SortBy _   '[] = '[]
    SortBy cmp (x ': xs) = InsertSortedBy cmp x (Sort xs)

  type family InsertSortedBy cmp (x :: *) (xs :: [*]) where
    InsertSortedBy _   x '[]       = x ': '[]
    InsertSortedBy cmp x (y ': ys) = InsertSorted' cmp (Cmp cmp x y) x y ys

  type family InsertSorted' cmp b x y ys where
    InsertSorted' _   'LT  x y ys = x ': (y ': ys)
    InsertSorted' cmp _    x y ys = y ': InsertSortedBy cmp x ys

  type family Sort xs where
    Sort xs = SortBy CmpName xs

  type family InsertSorted (x :: *) (xs :: [*]) where
    InsertSorted x xs = InsertSortedBy CmpName x xs

  type family (++) as bs where
    '[] ++ bs = bs
    (a : as) ++ bs = a : (as ++ bs)

  -- Nub only deletes sequences of duplicates
  type family Nub as where
    Nub    '[]       = '[]
    Nub (x : x : xs) = Nub (x : xs)
    Nub (x : xs)     = x ': Nub xs

  type family Has a bs where
    Has a '[]       = 'False
    Has a (a ': bs) = 'True
    Has a (b ': bs) = Has a bs

  type family DeleteOne a bs where
    DeleteOne a '[]      = '[]
    DeleteOne a (a ': t) = t
    DeleteOne a (h ': t) = h : DeleteOne a t

  type family DeleteAll a bs where
    DeleteAll a bs = If (Has a bs) (DeleteAll a (DeleteOne a bs)) bs

{-# LANGUAGE TypeFamilies, TypeOperators, UndecidableInstances #-}

module TypeLevel.List (
  Sort, InsertSorted, type (++), Nub, Has, DeleteOne
  ) where

  import GHC.TypeLits
  import GHC.Generics
  import Data.Type.Bool
  import TypeLevel.Reflection (TypeName)

  type family CmpName a b :: Ordering where
    CmpName a b = CmpSymbol (TypeName a) (TypeName b)

  -- insertion sort from https://kseo.github.io/posts/2017-01-30-type-level-insertion-sort.html
  type family Sort xs where
    Sort '[] = '[]
    Sort (x ': xs) = InsertSorted x (Sort xs)

  type family InsertSorted x xs where
    InsertSorted x '[] = x ': '[]
    InsertSorted x (y ': ys) = InsertSorted' (CmpName x y) x y ys

  type family InsertSorted' b x y ys where
    InsertSorted' 'LT  x y ys = x ': (y ': ys)
    InsertSorted' _    x y ys = y ': InsertSorted x ys

  type family (++) as bs where
    '[] ++ bs = bs
    (a : as) ++ bs = a : (as ++ bs)

  -- Nub only deletes sequences of duplicates
  type family Nub as where
    Nub    '[]       = '[]
    Nub (x : x : xs) = Nub (x : xs)
    Nub (x : xs)     = x ': Nub xs

  type family Has a bs where
    Has a '[]     = False
    Has a (a ': bs) = True
    Has a (b ': bs) = Has a bs

  type family DeleteOne a bs where
    DeleteOne a '[]      = '[]
    DeleteOne a (a ': t) = t
    DeleteOne a (h ': t) = h : DeleteOne a t

  type family DeleteAll a bs where
    DeleteAll a bs = If (Has a bs) (DeleteAll a (DeleteOne a bs)) bs

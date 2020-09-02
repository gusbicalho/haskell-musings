{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module NFold where

import GHC.TypeLits (KnownNat, type (-))

class (KnownNat n) => NFoldL n v z r | n v z -> r where
  nfoldl' :: (z -> v -> z) -> z -> r

instance
  {-# OVERLAPPING #-}
  (next ~ z) =>
  NFoldL 0 v z next
  where
  {-# INLINE nfoldl' #-}
  nfoldl' _ z = z

instance
  {-# OVERLAPPABLE #-}
  ( KnownNat i,
    NFoldL (i - 1) v z next,
    next2 ~ (v -> next)
  ) =>
  NFoldL i v z next2
  where
  {-# INLINE nfoldl' #-}
  nfoldl' f !z = \v -> nfoldl' @(i - 1) f (f z v)

-- >>> nfoldl' @0 (+) (0 :: Int)
-- 0
-- >>> nfoldl' @1 (+) (0 :: Int) 1
-- 1
-- >>> nfoldl' @2 (+) (0 :: Int) 1 2
-- 3

-- This actually folds left to compose a builder fn that folds right
-- and finally take the "zero" acc as final parameter, which is the parameter
-- for the builder fn
-- actual foldr takes the zero param before the list
{-# INLINE nfoldr_like #-}
nfoldr_like :: forall i v z next. (NFoldL i v (z -> z) next) => (v -> z -> z) -> next
nfoldr_like f = nfoldl' @i (\acc v -> acc . f v) id

-- >>> nfoldr_like @3 @Int (:) 1 2 3 []
-- [1,2,3]

-- Typeclass to compose a single arg fn g to another fn, after passing N args to that fn
class (KnownNat n) => ComposeAfterNArgs n g next next' | n g next -> next' where
  composeAfter :: g -> next -> next'

instance
  {-# OVERLAPPING #-}
  (next ~ a, next' ~ b) =>
  ComposeAfterNArgs 0 (a -> b) next next'
  where
  {-# INLINE composeAfter #-}
  composeAfter g a = g a

instance
  {-# OVERLAPPABLE #-}
  ( KnownNat i,
    ComposeAfterNArgs (i - 1) (a -> b) next2 next2',
    next ~ (v -> next2),
    next' ~ (v -> next2')
  ) =>
  ComposeAfterNArgs i (a -> b) next next'
  where
  {-# INLINE composeAfter #-}
  composeAfter g f = \v -> composeAfter @(i - 1) g (f v)

-- Actual foldr

class (KnownNat n) => NFoldR n v z r | n v z -> r where
  nfoldr :: (v -> z -> z) -> z -> r

instance
  {-# OVERLAPPING #-}
  (next ~ z) =>
  NFoldR 0 v z next
  where
  {-# INLINE nfoldr #-}
  nfoldr _ z = z

instance
  {-# OVERLAPPABLE #-}
  ( KnownNat i,
    ComposeAfterNArgs (i - 1) (z -> z) next next',
    NFoldR (i - 1) v z next,
    next2 ~ (v -> next')
  ) =>
  NFoldR i v z next2
  where
  {-# INLINE nfoldr #-}
  nfoldr f z v = composeAfter @(i - 1) (f v) (nfoldr @(i - 1) f z)

xs :: [Int]
xs = nfoldr @4 (:) ([] @Int) 1 2 3 4

ys :: [Int]
ys = nfoldr_like @4 (:) 1 2 3 4 ([] @Int)

six = nfoldr @4 (+) (0 :: Int) 1 2 3 4

sixlike = nfoldr_like @4 (+) 1 2 3 4 (0 :: Int)

-- >>> nfoldr @0 (:) []
-- []
-- >>> nfoldr @1 (:) [] 1
-- [1]
-- >>> nfoldr @3 (:) [] 1 2 3
-- [1,2,3]

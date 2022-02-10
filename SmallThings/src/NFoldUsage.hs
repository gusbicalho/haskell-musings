{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module NFoldUsage where

import NFold ( nfoldr_like, NFoldR(nfoldr) )

xs :: [Int]
xs = nfoldr @4 (:) ([] @Int) 1 2 3 4

ys :: [Int]
ys = nfoldr_like @4 (:) 1 2 3 4 ([] @Int)

six :: Int
six = nfoldr @4 (+) (0 :: Int) 1 2 3 4

sixlike :: Int
sixlike = nfoldr_like @4 (+) 1 2 3 4 (0 :: Int)

sum4 :: Int -> Int -> Int -> Int -> Int
sum4 = nfoldr @4 (+) (0 :: Int)

sum4_applied :: Int -> Int -> Int -> Int -> Int
sum4_applied a b c d = sum4 a b c d

sum4_ugly :: Int -> Int -> Int -> Int -> Int
sum4_ugly a b c d = nfoldr @4 (+) (0 :: Int) a b c d

sum4_like :: Int -> Int -> Int -> Int -> Int
sum4_like a b c d = nfoldr_like @4 (+) a b c d (0 :: Int)

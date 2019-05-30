module BinaryTree where

  data Tree a = L | T a (Tree a) (Tree a)
    deriving (Eq, Show)

  type TreeMissingBranch a = Tree a -> Tree a

  (-|) :: Tree a -> a -> TreeMissingBranch a
  l -| a = T a l
  infixr 3 -|

  (|-) :: TreeMissingBranch a -> Tree a -> Tree a
  la |- r = la r
  infixl 2 |-

  leaf a = T a L L

{-
>>> T 5 (T 3 (T 2 L L) (T 4 L L)) (T 7 (T 6 L L) (T 8 L L))
T 5 (T 3 (T 2 L L) (T 4 L L)) (T 7 (T 6 L L) (T 8 L L))

>>> (leaf 2 -| 3 |- leaf 4) -| 5 |- (leaf 6 -| 7 |- leaf 8)
T 5 (T 3 (T 2 L L) (T 4 L L)) (T 7 (T 6 L L) (T 8 L L))

-}

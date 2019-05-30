{-# LANGUAGE
    DataKinds, FlexibleContexts, FlexibleInstances, GADTs
  , KindSignatures, PolyKinds, TypeOperators, PostfixOperators
#-}

module HTree where

  data RoseTree a = R a [RoseTree a]
    deriving (Eq, Show)

  data HTreeList (ts :: [RoseTree *]) where
    HNil :: HTreeList '[]
    (:>:) :: HTree t -> HTreeList ts -> HTreeList (t ': ts)
  infixr 5 :>:

  (>.) :: HTree t -> HTreeList '[t]
  (>.) t = t :>: HNil
  infixl 3 >.

  data HTree (treeT :: RoseTree *) where
    HT :: a -> HTreeList ts -> HTree ('R a ts)

  instance (Eq a) => Eq (HTree (R a '[])) where
    (HT a HNil) == (HT b HNil) = a == b

  instance (Eq (HTreeList '[])) where
    HNil == HNil = True

  instance (Eq (HTree t), Eq (HTreeList ts)) => (Eq (HTreeList (t ': ts))) where
    (t :>: ts) == (u :>: us) = t == u && ts == us

  instance (Eq a, Eq (HTreeList (x ': xs))) => Eq (HTree (R a (x ': xs))) where
    (HT a as) == (HT b bs) = a == b && as == bs

  -- Show

  instance (Show a) => Show (HTree (R a '[])) where
    showsPrec d (HT a HNil) = showParen (d > appPrec) $
        showString "HT " .
        showsPrec (appPrec + 1) a . showString " HNil"
      where appPrec = 10

  instance (Show (HTreeList '[])) where
    showsPrec _ _ s = "HNil" ++ s

  instance (Show (HTree t), Show (HTreeList ts)) => (Show (HTreeList (t ': ts))) where
    showsPrec d (t :>: HNil) = showParen (d > upPrec) $
        showsPrec (upPrec + 1) t .
        showString " >."
      where upPrec = 3
    showsPrec d (t :>: ts) = showParen (d > upPrec) $
        showsPrec (upPrec + 1) t .
        showString " :>: " .
        showsPrec (upPrec + 1) ts
      where upPrec = 3

  instance (Show a, Show (HTreeList (x ': xs))) => Show (HTree (R a (x ': xs))) where
    showsPrec d (HT a children) = showParen (d > appPrec) $
        showString "HT " .
        showsPrec (appPrec + 1) a . showString " " .
        showsPrec (appPrec + 1) children
      where appPrec = 10

{-
>>> :set -XPostfixOperators
>>> newtype X = X Int deriving (Eq, Show)
>>> newtype Y = Y Int deriving (Eq, Show)
>>> newtype Z = Z Int deriving (Eq, Show)
>>> x = HT (X 1) HNil
>>> y = HT (Y 2) HNil
>>> z = HT (Z 3) HNil
>>> xyz      = HT "xyz" (x :>: y :>: (z >.))
>>> xyzUpper = HT "XYZ" (x :>: y :>: (z >.))
>>> xyzMeta  = HT "meta" (xyz :>: (xyz >.))
>>> xyz
>>> xyzMeta
>>> xyz == xyzUpper
>>> xyz == xyzMeta
HT "xyz" (HT (X 1) HNil :>: (HT (Y 2) HNil :>: (HT (Z 3) HNil >.)))
HT "meta" (HT "xyz" (HT (X 1) HNil :>: (HT (Y 2) HNil :>: (HT (Z 3) HNil >.))) :>: (HT "xyz" (HT (X 1) HNil :>: (HT (Y 2) HNil :>: (HT (Z 3) HNil >.))) >.))
False
<BLANKLINE>
<interactive>:5140:9-15: error:
    • Couldn't match type ‘[Char]’ with ‘X’
      Expected type: HTree ('R [Char] '[ 'R X '[], 'R Y '[], 'R Z '[]])
        Actual type: HTree
                       ('R
                          [Char]
                          '[ 'R [Char] '[ 'R X '[], 'R Y '[], 'R Z '[]],
                             'R [Char] '[ 'R X '[], 'R Y '[], 'R Z '[]]])
    • In the second argument of ‘(==)’, namely ‘xyzMeta’
      In the expression: xyz == xyzMeta
      In an equation for ‘it’: it = xyz == xyzMeta

-}

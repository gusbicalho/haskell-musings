{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FirstClassModules where

import Data.Kind (Constraint, Type)

-- Slide 2

class EQ a mdl where
  eq :: mdl -> a -> a -> Bool

-- Slide 3

data IntEQ = IntEQ

instance EQ Int IntEQ where
  eq _ a b = (a `mod` 10) == (b `mod` 10)

-- Slide 4

data IntModEQ10 = IntModEQ10

instance EQ Int IntModEQ10 where
  eq _ a b = (a `mod` 10) == (b `mod` 10)

-- Slide 5

data PairEQ eqA eqB = PairEQ eqA eqB

instance (EQ a eqA, EQ b eqB) => EQ (a, b) (PairEQ eqA eqB) where
  eq (PairEQ eqA eqB) (a1, b1) (a2, b2) =
    eq eqA a1 a2 && eq eqB b1 b2

moduleII :: PairEQ IntEQ IntEQ
moduleII = PairEQ IntEQ IntEQ

-- Slide 6

newtype IntModEQ = IntModEQ Int

instance EQ Int IntModEQ where
  eq (IntModEQ modulus) a b = (a `mod` modulus) == (b `mod` modulus)

intEQMod13 :: IntModEQ
intEQMod13 = IntModEQ 13

-- Slide 7

slide7simple :: ([Bool], [Bool])
slide7simple = do
  (map (f 0 13) eqs, map (f 0 17) eqs)
 where
  eqs = [IntModEQ 13, IntModEQ 17]
  f :: Int -> Int -> IntModEQ -> Bool
  f a b m = eq m a b

-- >>> slide7simple
-- ([True,False],[False,True])

-- The above only works because both EQs are IntModEQ
-- To allow mixing different types of modules, we need an existential
data Some (c :: Type -> Constraint) where
  Some :: c a => a -> Some c

slide7 :: ([Bool], [Bool], [Bool])
slide7 =
  ( map (f 0 10) eqs
  , map (f 0 13) eqs
  , map (f 0 17) eqs
  )
 where
  eqs :: [Some (EQ Int)]
  eqs = [Some IntModEQ10, Some $ IntModEQ 13, Some $ IntModEQ 17]
  f :: Int -> Int -> Some (EQ Int) -> Bool
  f a b (Some m) = eq m a b

-- >>> slide7
-- ([True,False,False],[False,True,False],[False,False,True])

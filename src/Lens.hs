{-# LANGUAGE
  AllowAmbiguousTypes, DataKinds,
  ExistentialQuantification, FlexibleInstances, MultiParamTypeClasses, RankNTypes,
  NamedFieldPuns, UndecidableInstances
#-}

module Lens where

import Data.Char
import Data.Type.Bool

newtype Const a v = Const a deriving (Eq, Show)
instance Functor (Const a) where
  fmap _ (Const a) = Const a
instance Monoid a => Applicative (Const a) where
  pure v = Const mempty
  (Const a1) <*> (Const a2) = Const $ a1 `mappend` a2
  
getConst :: Const a v -> a
getConst (Const a) = a

newtype Identity a = Identity {runIdentity :: a} deriving (Eq, Show)
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
instance Applicative Identity where
  pure = Identity
  (Identity f) <*> x = fmap f x

type Lens s a = forall f. Functor f => (a -> f a) -> s -> f s
data LensR s a = LensR { viewR :: s -> a
                       , overR :: (a -> a) -> s -> s }

-- over :: Lens s a -> (a -> a) -> s -> s
over ln f = runIdentity . ln (Identity . f)

-- set :: Lens s a -> s -> a -> s
set ln s a = over ln (const a) s
(.~) = set
infixr 3 .~

view ln = getConst . ln Const

lensToLensR :: Lens s a -> LensR s a
lensToLensR ln = LensR { viewR = view ln
                       , overR = over ln }

lensRToLens :: LensR s a -> Lens s a
lensRToLens LensR { viewR, overR } xform s = fmap rebox $ xform $ unbox
  where
    unbox = viewR s
    rebox a = overR (const a) s

type Traversal s a = forall f. Applicative f => (a -> f a) -> s -> f s

-- examples below


{--

(.) :: (b -> c) -> (a -> b) -> a -> c
type Lens s a = forall f. Functor f => (a -> f a) -> s -> f s

((undefined :: Lens p q) . (undefined :: Lens q r))

(b -> c) ~ ((q -> f q) -> p -> f p)
b        ~ q -> f q
c        ~ p -> f p

(a -> b) ~ (r -> f r) -> q -> f q
a        ~ r -> f r
b        ~ q -> f q

a -> c ~ (r -> f r) -> (p -> f p)
       ~ (r -> f r) -> p -> f p
       ~ Lens p r

--}

data Person = Person { _name    :: String
                     , _address :: Address
                     }
                     deriving (Eq, Show)
data Address = Address { _street :: String
                       }
                       deriving (Eq, Show)

name xform s =
  (\a -> s { _name = a })
  <$> xform (_name s)

address xform s =
  (\a -> s { _address = a })
  <$> xform (_address s)

street xform s =
  (\a -> s { _street = a })
  <$> xform (_street s)

both :: Monoid a => Traversal s a -> Traversal s a -> Traversal s a
both lna lnb xform s =
  pure (\a b -> (set lnb (set lna s a) b))
       <*> xform (view lna s)
       <*> xform (view lnb s)
(~&~) :: Monoid a => Traversal s a -> Traversal s a -> Traversal s a
lna ~&~ lnb = both lna lnb

nameAndStreet :: Traversal Person String
nameAndStreet xform p =
  (\n s -> (set (address.street) (set name p n) s))
  <$> xform (view name p) <*> xform (view (address.street) p)

nameAndStreet2 :: Traversal Person String
nameAndStreet2 = both name (address.street)

someone = (Person "Jack" (Address "road block"))

-- >>> view name someone
-- >>> view address someone
-- >>> view (address . street) someone
-- "Jack"
-- Address {_street = "road block"}
-- "road block"
--

-- >>> :set -XNamedFieldPuns
-- >>> let LensR { viewR } = (lensToLensR $ address . street) in viewR someone
-- "road block"
--

{-
>>> someone
>>> name .~ someone $ "Jane"
>>> address . street .~ someone $ "Baker Street"
>>> address.street .~ someone $ "Baker Street"
>>> over nameAndStreet (map toUpper) someone
>>> view (name ~&~ (address.street)) someone
>>> view (name ~&~ (address.street) ~&~ name) someone
Person {_name = "Jack", _address = Address {_street = "road block"}}
Person {_name = "Jane", _address = Address {_street = "road block"}}
Person {_name = "Jack", _address = Address {_street = "Baker Street"}}
Person {_name = "Jack", _address = Address {_street = "Baker Street"}}
Person {_name = "JACK", _address = Address {_street = "ROAD BLOCK"}}
"Jackroad block"
"Jackroad blockJack"

-}
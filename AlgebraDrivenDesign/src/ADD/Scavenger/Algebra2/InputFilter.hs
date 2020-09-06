{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module ADD.Scavenger.Algebra2.InputFilter where

import GHC.Generics (Generic)

data InputFilter i
  = Always
  | Never
  | And (InputFilter i) (InputFilter i)
  | Or (InputFilter i) (InputFilter i)
  | Not (InputFilter i)
  | Custom (CustomFilter i)
  deriving stock (Generic)

deriving instance Eq (CustomFilter i) => Eq (InputFilter i)

deriving instance Ord (CustomFilter i) => Ord (InputFilter i)

deriving instance Show (CustomFilter i) => Show (InputFilter i)

class HasFilter i where
  data CustomFilter i
  filterMatches :: CustomFilter i -> i -> Bool

matches :: HasFilter i => InputFilter i -> i -> Bool
matches Always _ = True
matches Never _ = False
matches (And f1 f2) i = matches f1 i && matches f2 i
matches (Or f1 f2) i = matches f1 i || matches f2 i
matches (Not f) i = not $ matches f i
matches (Custom f) i = filterMatches f i

custom :: CustomFilter i -> InputFilter i
custom = Custom

always :: InputFilter i
always = Always

-- Law "matches/always"
-- forall i.
--   matches always i = True

never :: InputFilter i
never = Never

-- Law "matches/never"
-- forall i.
--   matches never i = False

andF :: InputFilter i -> InputFilter i -> InputFilter i
andF Never _ = Never
andF _ Never = Never
andF Always b = b
andF a Always = a
andF a b = And a b

-- Law "matches/andF"
-- forall f1 f2 i.
--   matches (andF f1 f2) i = matches f1 i && matches f2 i

orF :: InputFilter i -> InputFilter i -> InputFilter i
orF Always _ = Always
orF _ Always = Always
orF Never b = b
orF a Never = a
orF a b = Or a b

-- Law "matches/orF"
-- forall f1 f2 i.
--   matches (orF f1 f2) i = matches f1 i || matches f2 i

notF :: InputFilter i -> InputFilter i
notF Always = Never
notF Never = Always
notF (Not a) = a
notF a = Not a

-- Law "matches/notF"
-- forall f i.
--   matches (notF f) i = not (matches f i)

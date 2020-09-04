{-# LANGUAGE TypeFamilies #-}
module ADD.Scavenger.Algebra.InputFilter where

data InputFilter i

class HasFilter i where
  type CustomFilter i
  filterMatches :: CustomFilter i -> i -> Bool

matches :: HasFilter i => InputFilter i -> i -> Bool
matches _f i = undefined (filterMatches undefined i)

custom :: HasFilter i => CustomFilter i -> InputFilter i
custom _ = undefined

always :: InputFilter i
always = undefined

-- Law "matches/always"
-- forall i.
--   matches always i = True

never :: InputFilter i
never = undefined

-- Law "matches/never"
-- forall i.
--   matches never i = False

andF :: InputFilter i -> InputFilter i -> InputFilter i
andF _ _ = undefined

-- Law "matches/andF"
-- forall f1 f2 i.
--   matches (andF f1 f2) i = matches f1 i && matches f2 i

orF :: InputFilter i -> InputFilter i -> InputFilter i
orF _ _ = undefined

-- Law "matches/orF"
-- forall f1 f2 i.
--   matches (orF f1 f2) i = matches f1 i || matches f2 i

notF :: InputFilter i -> InputFilter i
notF _ = undefined

-- Law "matches/notF"
-- forall f i.
--   matches (notF f) i = not (matches f i)

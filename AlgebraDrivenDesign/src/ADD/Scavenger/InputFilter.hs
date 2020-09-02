module ADD.Scavenger.InputFilter where

import ADD.Scavenger.Types

matches :: InputFilter -> Input -> Bool
matches _ _ = undefined

always :: InputFilter
always = undefined

-- Law "matches/always"
-- forall i.
--   matches always i = True

never :: InputFilter
never = undefined

-- Law "matches/never"
-- forall i.
--   matches never i = False

andF :: InputFilter -> InputFilter -> InputFilter
andF _ _ = undefined

-- Law "matches/andF"
-- forall f1 f2 i.
--   matches (andF f1 f2) i = matches f1 i && matches f2 i

orF :: InputFilter -> InputFilter -> InputFilter
orF _ _ = undefined

-- Law "matches/orF"
-- forall f1 f2 i.
--   matches (orF f1 f2) i = matches f1 i || matches f2 i

notF :: InputFilter -> InputFilter
notF _ = undefined

-- Law "matches/notF"
-- forall f i.
--   matches (notF f) i = not (matches f i)

-- Specific filters

locWithin :: Point -> Distance -> InputFilter
locWithin _ _ = undefined

-- Law "matches/locWithin"
-- forall poi d p.
--   matches (locWithin poi d) (location p)
--   = within poi p d
-- Law "matches/locWithin not location"
-- forall poi d i.
--   not(isLocation i) =>
--     matches (locWithin poi d) i
--     = False

photoWithin :: Point -> Distance -> InputFilter
photoWithin _ _ = undefined

-- Law "matches/photoWithin"
-- forall poi d p pic.
--   matches (photoWithin poi d) (photo p pic)
--   = within poi p d
-- Law "matches/photoWithin not photo"
-- forall poi d i.
--   not(isPhoto i) =>
--     matches (photoWithin poi d) i
--     = False

photoAbove :: Altitude -> InputFilter
photoAbove _ = undefined

-- Law "matches/photoAbove"
-- forall a p pic.
--   matches (photoAbove a) (photo p pic)
--   = aboveAltitude p a
-- Law "matches/photoAbove not photo"
-- forall a i.
--   not(isPhoto i) =>
--     matches (photoAbove a) i
--     = False

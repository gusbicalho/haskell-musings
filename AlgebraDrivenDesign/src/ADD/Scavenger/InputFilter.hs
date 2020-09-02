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

photoWithin :: Point -> Distance -> InputFilter
photoWithin _ _ = undefined

photoAbove :: Altitude -> InputFilter
photoAbove _ = undefined

afterTime :: Time -> InputFilter
afterTime _ = undefined

module ADD.Scavenger.Clue where

import ADD.Scavenger.Types

hint :: String -> Clue
hint _ = undefined

noClue :: Clue
noClue = undefined

sub :: Clue -> Clue -> Clue
sub _ _ = undefined

-- Law "sub:identity"
-- forall c.
--   sub noClue c = c = sub c noClue
-- Law "sub:associative"
-- forall k1 k2 k3.
--   sub k1 (sub k2 k3) = sub (sub k1 k2) k3

toList :: Clue -> [String]
toList _ = undefined

-- Law "toList/noClue"
-- forall s.
--   toList noClue = []
-- Law "toList/hint"
-- forall s.
--   toList (hint s) = [s]
-- Law "toList/sub"
-- forall k1 k2.
--   toList (sub k1 k2) = toList k1 <> toList k2

seen :: ClueState
seen = undefined

completed :: ClueState
completed = undefined

failed :: ClueState
failed = undefined

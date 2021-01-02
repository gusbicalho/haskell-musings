{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Examples where

import Control.Arrow (Arrow ((&&&)))
import Objects qualified as O

newtype ToInt a = ToInt {toInt :: a -> Int}
newtype ToWord a = ToWord {toWord :: a -> Word}
newtype ToString a = ToString {toString :: a -> String}

someX :: O.Object '[ToInt, ToWord, ToString]
someX =
  O.object $
    O.Implement ToInt{toInt = id}
      . O.Implement ToWord{toWord = fromIntegral}
      . O.Implement ToString{toString = \a -> show a <> "!!!"}
      $ O.ObjectData 42

foo :: (O.Implements ToInt t, O.Implements ToWord t, O.Implements ToString t) => t -> (Int, (Word, String))
foo = O.call toInt &&& O.call toWord &&& O.call toString

-- >>> foo someX
-- (42,(42,"42!!!"))

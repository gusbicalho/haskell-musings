module RealWorldHaskell.Ch16.FormParse where

import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Numeric(readHex)
import Control.Applicative

type Pair = (String, Maybe String)

pQuery :: CharParser st [Pair]
pQuery = pPair `sepBy` char '&'

-- >>> parse pQuery "" "foo=bar&a%21=b+c&q"
-- Right [("foo",Just "bar"),("a!",Just "b c"),("q",Nothing)]
--

pPair :: CharParser st Pair
pPair = liftA2 (,) (many1 pChar)
                   (optionMaybe (char '=' *> many pChar))

pChar :: CharParser st Char
pChar = oneOf urlBaseChars
    <|> (' ' <$ char '+')
    <|> pHex

urlBaseChars :: [Char]
urlBaseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),"

pHex :: CharParser st Char
pHex = char '%' *> liftA2 hexify hexDigit hexDigit
  where hexify a b = toEnum . fst . head . readHex $ [a,b]

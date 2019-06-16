-- {-# LANGUAGE
--     NoMonomorphismRestriction
-- #-}
module RealWorldHaskell.Ch16.JSONParse where

import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Control.Applicative
import Numeric(readSigned, readFloat, readHex)

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
            deriving (Eq, Ord, Show)

-- >>> parse pText "" "[\"hue\\u0021\\n\", false, null, 3.14, true, [true, 3.15]]"
-- Right (JArray [JString "hue!\n",JBool False,JNull,JNumber 3.14,JBool True,JArray [JBool True,JNumber 3.15]])
--

pText :: CharParser st JValue
pText = spaces *> text
    <?> "JSON text"
  where text = JObject <$> pObject
           <|> JArray <$> pArray

pValue :: CharParser st JValue
pValue = value <* spaces
  where value = choice [ JNull <$ string "null"
                       , JBool <$> pBool
                       , JArray <$> pArray
                       , JObject <$> pObject
                       , JNumber <$> pNumber
                       , JString <$> pString
                       ]
                <?> "JSON value"

pArray :: CharParser st [JValue]
pArray = pSeries '[' pValue ']'

pObject :: CharParser st [(String, JValue)]
pObject = pSeries '{' keyval '}'
  where keyval = liftA2 (,) (pString <* spaces <* char ':' <* spaces)
                            pValue

pSeries :: Char -> CharParser st a -> Char -> CharParser st [a]
pSeries left parser right =
  between (char left <* spaces) (char right) $
          (parser <* spaces) `sepBy` (char ',' <* spaces)

pBool :: CharParser st Bool
pBool = (True  <$ string "true")
    <|> (False <$ string "false")

pNumber :: CharParser st Double
pNumber = do s <- getInput
             case readSigned readFloat s of
               [(n, s')] -> n <$ setInput s'
               _         -> empty

pString :: CharParser st String
pString = between (char '"') (char '"') (many jchar)
  where jchar = char '\\' *> (pEscape <|> pUnicode)
            <|> satisfy (`notElem` "\"\\")

pEscape :: CharParser st Char
pEscape = choice (zipWith decode "bnfrt\\\"/" "\b\n\f\r\t\\\"/")
  where decode :: Char -> Char -> CharParser st Char
        decode c r = r <$ char c

pUnicode :: CharParser st Char
pUnicode = char 'u' *> (decode <$> count 4 hexDigit)
    where decode x = toEnum code
              where ((code,_):_) = readHex x

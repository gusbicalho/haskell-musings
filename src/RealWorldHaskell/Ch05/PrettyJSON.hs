module RealWorldHaskell.Ch05.PrettyJSON
  (
    renderJValue
  ) where

import RealWorldHaskell.Ch05.SimpleJSON (JValue(..))
import RealWorldHaskell.Ch05.Prettify ( Doc, char, double, fsep, hcat
                                      , punctuate, text, compact, pretty)
import Data.Maybe (mapMaybe)
import Data.Bits
import Data.Char (ord)
import Numeric (showHex)

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

enclose :: Char -> Char -> Doc -> Doc
enclose pre suf content = char pre <> content <> char suf

oneChar :: Char -> Doc
oneChar c = head . mapMaybe ($ c) $ [ maybeSimpleEscape
                                    , maybeHexEscape
                                    , Just . char]

maybeSimpleEscape :: Char -> Maybe Doc
maybeSimpleEscape c = text <$> lookup c simpleEscapes

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, ['\\',b])

maybeHexEscape :: Char -> Maybe Doc
maybeHexEscape c = if mustEscape then Just (hexEscape c) else Nothing
  where mustEscape = c < ' ' || c == '\x7f' || c > '\xff'

smallHex :: Int -> Doc
smallHex x  = text "\\u"
           <> text (replicate (4 - length h) '0')
           <> text h
    where h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b = n .&. 0x3ff

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise   = astral (d - 0x10000)
  where d = ord c

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close
                       . fsep . punctuate (char ',') . map item

renderJValue :: JValue -> Doc
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull         = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str
renderJValue (JArray ary)  = series '[' ']' renderJValue ary
renderJValue (JObject pairs) = series '{' '}' renderPair pairs
  where renderPair (key, value) = string key <> text ": " <> renderJValue value

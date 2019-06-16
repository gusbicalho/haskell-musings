module RealWorldHaskell.Ch16.CSV where

import Text.ParserCombinators.Parsec

parseCSV :: String -> Either ParseError [[String]]
parseCSV = parse csvFile "(unknown)"

-- >>> parseCSV "asd,qwe,zxc\nwer,sdf,xcv,\"a\nsd,q\"\"we\",a\r\nx\n"
-- Right [["asd","qwe","zxc"],["wer","sdf","xcv","a\nsd,q\"we","a"],["x"]]
--
-- >>> parseCSV "line1\r\nline2\nline3\n\rline4\rline5\n"
-- Right [["line1"],["line2"],["line3"],["line4"],["line5"]]
--

{- A CSV file contains 0 or more lines, each of which is terminated
   by the end-of-line character (eol). -}
csvFile :: GenParser Char st [[String]]
csvFile = line `endBy` eol
  where line = cell `sepBy` (char ',')

cell :: GenParser Char st String
cell = quotedCell <|> many (noneOf ",\r\n")

quotedCell :: GenParser Char st String
quotedCell = do
  _ <- char '"'
  content <- many quotedChar
  _ <- char '"' <?> "quote at end of cell"
  return content

quotedChar :: GenParser Char st Char
quotedChar = noneOf [quote]
         <|> try (string "\"\"" >> return quote)
  where quote = '"'

eol :: GenParser Char st String
eol = try (string "\r\n")
  <|> try (string "\n\r")
  <|> string "\r"
  <|> string "\n"
  <?> "end of line"

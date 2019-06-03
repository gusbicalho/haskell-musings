module RealWorldHaskell.Ch08.Glob ( globToRegex, matchesGlob, isGlob ) where

import Text.Regex.Posix ( (=~) )

matchesGlob :: String -> String -> Either GlobError Bool
matchesGlob glob s = fmap (s =~) (globToRegex glob) :: Either GlobError Bool

globToRegex :: String -> Either GlobError String
globToRegex = (fmap anchored) . globToRegex'

isGlob :: String -> Bool
isGlob = any (`elem` "[*?")

type GlobError = String

globToRegex' :: String -> Either GlobError String
globToRegex' ""                   = Right ""
globToRegex' ('*' : cs)           = (".*"          ++) <$> globToRegex' cs
globToRegex' ('?' : cs)           = ('.'           : ) <$> globToRegex' cs
globToRegex' ('[' : '!' : c : cs) = (("[^" ++ [c]) ++) <$> charClass cs
globToRegex' ('[' :       c : cs) = (['[', c]      ++) <$> charClass cs
globToRegex' ('[' : _)            = unterminatedError
globToRegex' (c   : cs)           = (escape c      ++) <$> globToRegex' cs

charClass :: String -> Either GlobError String
charClass (']' : cs) = (']' :) <$> globToRegex' cs
charClass (c   : cs) = (c   :) <$> charClass cs
charClass ""         = unterminatedError

unterminatedError :: Either GlobError a
unterminatedError = Left "unterminated char class"

anchored :: String -> String
anchored s = '^' : s ++ "$"

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
    where regexChars = "\\+()^$.{}]|"

{-
>>> testGlob glob expect = let r = globToRegex glob in (r, r == expect)
>>> testGlob "a*b*.hs" "^a.*b.*\\.hs$"
>>> testGlob "f??.c" "^f..\\.c$"
("^a.*b.*\\.hs$",True)
("^f..\\.c$",True)

-}

{-
>>> "abcabc" =~ "a.cb" :: Int
>>> "abcaxc" =~ "a.c" :: Int
0
2

>>> "abcabc" =~ "a.cb" :: Bool
>>> "abcaxc" =~ "a.c" :: Bool
False
True

>>> "abcabc" =~ "a.cb" :: [[String]]
>>> "abcaxc" =~ "a.c" :: [[String]]
[]
[["abc"],["axc"]]

>>> pat = "(foo[a-z]*bar|quux)"
>>> "before foodiebar after" =~ pat :: [[String]]
[["foodiebar","foodiebar"]]

>>> "before foodiebar after" =~ pat :: (String, String, String)
("before ","foodiebar"," after")

>>> "no match here" =~ pat :: (String,String,String)
("no match here","","")

>>> "before foodiebar after" =~ pat :: (String, String, String, [String])
("before ","foodiebar"," after",["foodiebar"])

>>> "before foodiebar after" =~ pat :: (Int, Int)
(7,9)

>>> "xablau" =~ pat :: (Int, Int)
(-1,0)

>>> import Text.Regex.Posix ( (=~), getAllMatches )
>>> getAllMatches $ "before foodiebar after" =~ pat :: [(Int, Int)]
[(7,9)]

>>> import Text.Regex.Posix ( (=~), getAllMatches )
>>> getAllMatches $ "xablau" =~ pat :: [(Int, Int)]
[]

-}

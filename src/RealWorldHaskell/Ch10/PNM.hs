{-# LANGUAGE
    RecordWildCards
  , NamedFieldPuns
  , BlockArguments
#-}

module RealWorldHaskell.Ch10.PNM
  ( parse
  , parseAll
  , Parse()
  , identity
  ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char ( isSpace )
import Control.Monad ( when )
import Data.Int ( Int64 )

data ParseState = ParseState {
      string :: L.ByteString
    , offset :: Int64
    } deriving (Show)

modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initState newOffset =
    initState { offset = newOffset }

type ParseFn o = ParseState -> Either String (o, ParseState)
newtype Parse o = Parse { runParse :: ParseFn o }

parseAll :: Parse o -> L.ByteString -> [o]
parseAll parser input = fst $ loop [] (ParseState input 0)
  where loop results state =
          case runParse parser state of
            Left  _               -> (results, state)
            Right (result, state') -> loop (result : results) state'

parse :: Parse o -> L.ByteString -> Either String o
parse parser input = fst <$> runParse parser (ParseState input 0)

identity :: a -> Parse a
identity a = Parse $ \s -> Right (a, s)

-- >>> parse (identity "foo") undefined
-- Right "foo"
--

data Greymap = Greymap {
      greyWidth :: Int
    , greyHeight :: Int
    , greyMax :: Int
    , greyData :: L.ByteString
    } deriving (Eq)

instance Show Greymap where
  show Greymap { .. } = "Greymap "
                        ++ show greyWidth ++ "x" ++ show greyHeight ++ " "
                        ++ show greyMax

-- >>> parse parseP5 (L8.pack "P5 3 3 255 222333444")
-- Right Greymap 3x3 255
--

parseP5 :: Parse Greymap
parseP5 = Parse $ \s -> do
  (_,       s) <- runParse (matchHeader (L8.pack "P5")) s
  (_,       s) <- runParse skipSpace s
  (width,   s) <- runParse getNat s
  (_,       s) <- runParse skipSpace s
  (height,  s) <- runParse getNat s
  (_,       s) <- runParse skipSpace s
  (maxGrey, s) <- runParse getNat s
  when (maxGrey > 255) $ Left "Invalid maxGrey"
  (_,       s) <- runParse (getBytes 1) s
  (bitmap,  s) <- runParse (getBytes (width * height)) s
  return (Greymap width height maxGrey bitmap, s)
  -- case matchHeader (L8.pack "P5") s of
  --   Nothing -> Nothing
  --   Just (_, s1) ->
  --     case getNat s1 of
  --       Nothing -> Nothing
  --       Just (width, s2) ->
  --         case getNat (L8.dropWhile isSpace s2) of
  --           Nothing -> Nothing
  --           Just (height, s3) ->
  --             case getNat (L8.dropWhile isSpace s3) of
  --               Nothing -> Nothing
  --               Just (maxGrey, s4)
  --                 | maxGrey > 255 -> Nothing
  --                 | otherwise ->
  --                     case getBytes 1 s4 of
  --                       Nothing -> Nothing
  --                       Just (_, s5) ->
  --                         case getBytes (width * height) s5 of
  --                           Nothing -> Nothing
  --                           Just (bitmap, s6) ->
  --                             Just (Greymap width height maxGrey bitmap, s6)

matchHeader :: L.ByteString -> Parse ()
matchHeader header = Parse matchHeader'
  where matchHeader' ParseState { string, offset } =
          case L.splitAt headerLen string of
            (header', rest) | header == header' ->
                                Right ((), ParseState rest (offset + headerLen))
                            | otherwise -> Left "Invalid header"
          where headerLen = L8.length header

skipSpace :: Parse ()
skipSpace = Parse skipSpace'
  where skipSpace' ParseState { string, offset } =
            Right ((), ParseState (L.drop spaceCount string) (offset + spaceCount))
          where spaceCount = L.length . L8.takeWhile isSpace $ string

getNat :: Parse Int
getNat = Parse $ \ParseState { string, offset } -> do
  (num, rest) <- maybe (Left "Failed to parse int") Right . L8.readInt $ string
  when (num <= 0) $ Left "Non-positive number"
  return (fromIntegral num, ParseState rest (offset + L.length rest))

getBytes :: Int -> Parse L.ByteString
getBytes n = Parse $ \ParseState { string, offset } ->
  let count         = fromIntegral n
      (bytes, rest) = L.splitAt count string
  in if count == L.length bytes
     then Right (bytes, ParseState rest (offset + count))
     else Left "Unexpected end of input"

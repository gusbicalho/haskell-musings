{-# LANGUAGE
    RecordWildCards
  , NamedFieldPuns
  , BlockArguments
#-}

module RealWorldHaskell.Ch10.Parse
  ( parse
  , parseAll
  , Parse()
  , identity
  , parseN
  , parseByte
  ) where

import qualified Data.ByteString.Lazy as L
import Data.Int ( Int64 )
import Data.Word ( Word8 )
import Data.Char ( chr, isSpace, isDigit )

data ParseState = ParseState {
      string :: L.ByteString
    , offset :: Int64
    } deriving (Show)

type ErrorMessage = String

type ParseFn o = ParseState -> Either ErrorMessage (o, ParseState)
newtype Parse o = Parse { runParse :: ParseFn o }

parseAll :: Parse o -> L.ByteString -> [o]
parseAll parser input = fst $ loop [] (ParseState input 0)
  where loop results state =
          case runParse parser state of
            Left  _               -> (results, state)
            Right (result, state') -> loop (result : results) state'

parse :: Parse o -> L.ByteString -> Either ErrorMessage o
parse parser input = fst <$> runParse parser (ParseState input 0)

identity :: a -> Parse a
identity a = Parse $ \s -> Right (a, s)
-- >>> parse (identity "foo") undefined
-- Right "foo"
--

(==>) :: Parse a -> (a -> Parse b) -> Parse b
parseA ==> parseBFactory = Parse $ \state -> do
  (resA, state') <- runParse parseA state
  runParse (parseBFactory resA) state'

(==>&) :: Parse a -> Parse b -> Parse b
parseA ==>& parseB = parseA ==> const parseB

instance Functor Parse where
  fmap f parser = parser ==> \res -> identity (f res)

getState :: Parse ParseState
getState = Parse $ \s -> Right (s, s)
-- >>> parse getState (L8.pack "asd")
-- Right (ParseState {string = "asd", offset = 0})
--

putState :: ParseState -> Parse ()
putState s = Parse $ \_ -> Right ((), s)
-- >>> parse (putState undefined) undefined
-- Right ()
--

bail :: ErrorMessage -> Parse a
bail errorMsg = Parse $ \st ->
  Left $ "byte offset "
       ++ show (offset st) ++ ": "
       ++ errorMsg

assert :: Bool -> ErrorMessage -> Parse ()
assert True  _        = identity ()
assert False errorMsg = bail errorMsg

liftParse2 :: (a -> b -> c) -> Parse a -> Parse b -> Parse c
liftParse2 f parseA parseB =
  parseA ==> \resA ->
    parseB ==> \resB ->
      identity (f resA resB)

liftParse :: (a -> b -> c) -> Parse a -> b -> Parse c
liftParse f parseA b =
  parseA ==> \resA ->
      identity (f resA b)

consP :: Parse a -> Parse [a] -> Parse [a]
consP = liftParse2 (:)

parseN :: Parse a -> Int -> Parse [a]
parseN _      0 = identity []
parseN parser n = parser `consP` parseN parser (n - 1)
-- >>> parse (parseN parseChar 2) (L8.pack "Asd")
-- Right "As"
--

parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith converter predicate = loop
  where loop = peekByte ==> \maybeByte ->
          case predicate . converter <$> maybeByte of
            Just True -> parseByte ==> \byte -> (converter byte :) <$> loop
            _         -> identity []

parseByte :: Parse Word8
parseByte = getState ==> \initState ->
  case L.uncons (string initState) of
    Nothing ->
        bail "no more input"
    Just (byte,remainder) ->
        putState newState ==> \_ ->
        identity byte
      where newState = initState { string = remainder,
                                    offset = newOffset }
            newOffset = offset initState + 1
-- >>> parse parseByte (L8.pack "Asd")
-- Right 65
--
-- >>> parse parseByte (L8.pack "")
-- Left "byte offset 0: no more input"
--

w2c :: Integral t => t -> Char
w2c = chr . fromIntegral

parseChar = w2c <$> parseByte
-- >>> parse parseChar (L8.pack "Asd")
-- Right 'A'
--
-- >>> parse parseChar (L8.pack "")
-- Left "byte offset 0: no more input"
--

peekByte :: Parse (Maybe Word8)
peekByte = fmap fst . L.uncons . string <$> getState

peekChar :: Parse (Maybe Char)
peekChar = fmap w2c <$> peekByte

parseBytes :: Int -> Parse L.ByteString
parseBytes n =
    getState ==> \st ->
    let n' = fromIntegral n
        (h, t) = L.splitAt n' (string st)
        st' = st { offset = offset st + L.length h, string = t }
    in putState st' ==>&
       assert (L.length h == n') "end of input" ==>&
       identity h
-- >>> parse (parseBytes 2) (L8.pack "Asd")
-- Right "As"
--
-- >>> parse (parseBytes 4) (L8.pack "Asd")
-- Left "byte offset 3: end of input"
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

parseRawPGM :: Parse Greymap
parseRawPGM =
    parseWhileWith w2c notWhite ==> \header -> skipSpaces ==>&
    assert (header == "P5") "invalid raw header" ==>&
    parseNat ==> \width -> skipSpaces ==>&
    parseNat ==> \height -> skipSpaces ==>&
    parseNat ==> \maxGrey ->
    assert (maxGrey <= 255) ("Invalid maxGrey " ++ show maxGrey) ==>&
    parseByte ==>&
    parseBytes (width * height) ==> \bitmap ->
    identity (Greymap width height maxGrey bitmap)
  where notWhite = (`notElem` " \r\n\t")

-- >>> parse parseRawPGM (L8.pack "P5 3 3 255 222333444")
-- Right Greymap 3x3 255
--

skipSpaces :: Parse ()
skipSpaces = const () <$> parseWhileWith w2c isSpace

parseNat :: Parse Int
parseNat = parseWhileWith w2c isDigit ==> validate
  where validate []     = bail "no more input"
        validate digits =
          let n = read digits
          in if n < 0
             then bail "integer undeflow"
             else identity n

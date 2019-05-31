module RealWorldHaskell.Ch05.Prettify
  (
    Doc(), empty, line, char, double, fsep, hcat, punctuate, text, compact, pretty
  ) where

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show,Eq)

instance Semigroup Doc where
  Empty <> b     = b
  a     <> Empty = a
  a     <> b     = Concat a b

instance Monoid Doc where
  mempty = empty

empty :: Doc
empty = Empty

line :: Doc
line = Line

char :: Char -> Doc
char = Char

text :: String -> Doc
text "" = empty
text s = Text s

double :: Double -> Doc
double num = text (show num)

hcat :: [Doc] -> Doc
hcat = foldr (<>) mempty

fsep :: [Doc] -> Doc
fsep = foldr (</>) mempty

(</>) :: Doc -> Doc -> Doc
a </> b = a <> softline <> b

-- >>> flatten $ fsep [text "hue", text "qwe"]
-- Concat (Text "hue") (Concat (Char ' ') (Concat (Text "qwe") (Char ' ')))
--

softline :: Doc
softline = group line

group :: Doc -> Doc
group d = Union (flatten d) d

flatten :: Doc -> Doc
flatten (Concat x y) = Concat (flatten x) (flatten y)
flatten Line         = Char ' '
flatten (Union x _)  = flatten x
flatten other        = other

punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ []     = []
punctuate _ [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

compact :: Doc -> String
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) =
              case d of
                Empty        -> transform ds
                Char c       -> c : transform ds
                Text s       -> s ++ transform ds
                Line         -> '\n' : transform ds
                a `Concat` b -> transform (a:b:ds)
                _ `Union` b  -> transform (b:ds)

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
    where best col (d:ds) =
              case d of
                Empty        -> best col ds
                Char c       -> c :  best (col + 1) ds
                Text s       -> s ++ best (col + length s) ds
                Line         -> '\n' : best 0 ds
                a `Concat` b -> best col (a:b:ds)
                a `Union` b  -> nicest col (best col (a:ds))
                                           (best col (b:ds))
          best _ _ = ""

          nicest col a b | (width - least) `fits` a = a
                         | otherwise                = b
                         where least = min width col

fits :: (Ord t, Num t) => t -> String -> Bool
fits w _ | w < 0 = False
fits _ ""        = True
fits _ ('\n':_)  = True
fits w (_:cs)    = (w - 1) `fits` cs

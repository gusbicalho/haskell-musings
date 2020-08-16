import Data.Ratio
import Data.List (sortBy)

-- Ex 01/02
myLength :: (Num b) => [a] -> b
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- >>> myLength [1..10] == length [1..10]
-- True
--

-- Ex 03
mean :: (Fractional a) => [a] -> a
mean xs = mySum xs / myLength xs
  where mySum []     = 0
        mySum (x:xs) = x + mySum xs

-- >>> mean [1,2,3,4]
-- 2.5
--

-- Ex 04
palindromize :: [a] -> [a]
palindromize xs = xs ++ reverse [] xs
  where reverse reversed []     = reversed
        reverse reversed (x:xs) = reverse (x:reversed) xs
-- >>> palindromize []
-- []
--
-- >>> palindromize [1..5]
-- [1,2,3,4,5,5,4,3,2,1]
--

-- Ex 05
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse [] xs
  where reverse reversed []     = reversed
        reverse reversed (x:xs) = reverse (x:reversed) xs

-- >>> isPalindrome []
-- True
--
-- >>> isPalindrome [1, 2, 3, 3, 2, 1]
-- True
--
-- >>> isPalindrome [1, 2, 3]
-- False
--

-- Ex 06
sortBySublistLength :: [[a]] -> [[a]]
sortBySublistLength = sortBy cmp
  where cmp []     [] = EQ
        cmp xs     [] = GT
        cmp []     ys = LT
        cmp (_:xs) (_:ys) = cmp xs ys

-- running the code below with `take 6` hangs because of the infinite list
-- >>> take 5 $ sortBySublistLength [[1..], [1,2], [2], [1..4], [3], [1]]
-- [[2],[3],[1],[1,2],[1,2,3,4]]
--

-- Ex 07
intersperse :: a -> [[a]] -> [a]
intersperse _ []  = []
intersperse _ [x] = x
intersperse y (x:xs) = x ++ (y : intersperse y xs)

-- >>> intersperse ',' []
-- ""
--
-- >>> intersperse ',' ["foo"]
-- "foo"
--
-- >>> intersperse ',' ["foo", "bar", "baz"]
-- "foo,bar,baz"
--

-- Ex 08
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

height :: (Num b, Ord b) => Tree a -> b
height Empty = 0
height (Node _ l r) = 1 + max (height l) (height r)

-- >>> height $ Node 1 Empty Empty
-- 1
--
-- >>> height $ Node 1 (Node 3 Empty Empty) (Node 5 (Node 4 Empty Empty) Empty)
-- 3
--

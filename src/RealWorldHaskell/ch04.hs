module Ch04 where

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x
-- >>> (safeHead [], safeHead [1], safeHead [1..])
-- (Nothing,Just 1,Just 1)
--

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs
-- >>> (safeTail [], safeTail [1], take 3 <$> safeTail [1..])
-- (Nothing,Just [],Just [2,3,4])
--

safeLast :: [a] -> Maybe a
safeLast []     = Nothing
safeLast [x]    = Just x
safeLast (x:xs) = safeLast xs
-- >>> (safeLast [], safeLast [1], safeLast [1,2,3,4])
-- (Nothing,Just 1,Just 4)
--

safeInit :: [a] -> Maybe [a]
safeInit = init []
  where init _   []     = Nothing
        init acc [x]    = Just $ reverse acc
        init acc (x:xs) = init (x : acc) xs
-- >>> (safeInit [], safeInit [1], safeInit [1,2,3,4])
-- (Nothing,Just [],Just [1,2,3])
--

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith pred = loop
  where loop []     = []
        loop (x:xs) = loop' x $ break pred xs
        loop' x (ys, zs) =  (x : ys) : loop zs
-- >>> splitWith odd [4, 6, 1, 2, 2, 3, 3, 4, 6, 7]
-- [[4,6],[1,2,2],[3],[3,4,6],[7]]
--

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr step zero []     = zero
myFoldr step zero (x:xs) = step x (myFoldr step zero xs)

takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 pred (x:xs) | pred x = x : takeWhile1 pred xs
takeWhile1 _    _               = []
-- >>> takeWhile1 odd [1, 3, 5, 4, 6]
-- >>> takeWhile1 odd [2, 4]
-- [1,3,5]
-- []
--

takeWhile2 :: (a -> Bool) -> [a] -> [a]
takeWhile2 pred = foldr step []
  where step x ys | pred x    = x : ys
                  | otherwise =     ys
-- >>> takeWhile2 odd [1, 3, 5, 4, 6]
-- >>> takeWhile2 odd [2, 4]
-- [1,3,5]
-- []
--

tails []         = [[]]
tails xs@(_:xs') = xs : tails xs'
-- >>> tails "tails"
-- ["tails","ails","ils","ls","s",""]
--

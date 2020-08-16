
module WC where

  main :: IO ()
  main = interact wordCount
    where
      wordCount input = foldl1 (++) $ map (process input) counts 
      process input f = show (f input) ++ "\n"
      counts = [length
               ,length . words
               ,length . lines]

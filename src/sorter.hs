module Main where

import Data.List (sort)

pipeLinesStd :: ([String] -> [String]) -> IO ()
pipeLinesStd f = interact (unlines . f . lines)

main :: IO ()
main = pipeLinesStd sort

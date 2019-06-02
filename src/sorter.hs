module Main where

import System.IO
import Data.List (sort)

doall :: (Monad m) => (a -> m b) -> [a] -> m ()
doall _ []     = return ()
doall f (x:xs) = f x >> doall f xs

pipeLines :: Handle -> Handle -> ([String] -> [String]) -> IO ()
pipeLines ih oh f = do iStr <- hGetContents ih
                       let output = f $ lines iStr
                       doall (hPutStrLn oh) output

main :: IO ()
main = pipeLines stdin stdout processLines

processLines :: [String] -> [String]
processLines = sort

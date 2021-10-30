{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Bench.Counter qualified as Counter
import Control.Monad (when)
import Data.Word (Word8)
import System.Environment (getArgs)
import System.Exit (die, exitFailure)

counterCases :: [([Char], IO Word8)]
counterCases =
  [ ("IOFreeData", Counter.benchIOFreeData)
  , ("StateFreeData", pure Counter.benchStateFreeData)
  , ("IOFreeChurch", Counter.benchIOFreeChurch)
  , ("StateFreeChurch", pure Counter.benchStateFreeChurch)
  , ("IOFreeFinalReader", Counter.benchIOFreeFinalReader)
  , ("StateFreeFinalReader", pure Counter.benchStateFreeFinalReader)
  , ("IOFreeFinalClassy", Counter.benchIOFreeFinalClassy)
  , ("StateFreeFinalClassy", pure Counter.benchStateFreeFinalClassy)
  , ("IOFreerData", Counter.benchIOFreerData)
  , ("StateFreerData", pure Counter.benchStateFreerData)
  , ("IOFreerChurch", Counter.benchIOFreerChurch)
  , ("StateFreerChurch", pure Counter.benchStateFreerChurch)
  , ("IOFreerFinalReader", Counter.benchIOFreerFinalReader)
  , ("StateFreerFinalReader", pure Counter.benchStateFreerFinalReader)
  , ("IOFreerFinalClassy", Counter.benchIOFreerFinalClassy)
  , ("StateFreerFinalClassy", pure Counter.benchStateFreerFinalClassy)
  ]

main :: IO ()
main = do
  (bench : impl : _) <- getArgs
  putStrLn $ bench <> " - " <> impl
  case bench of
    "counter" ->
      check 17 $ case lookup impl counterCases of
        Just counterBench -> counterBench
        Nothing -> die "Unknown impl for counter"
    _ -> die "Unknown benchmark"
 where
  check expected bench = do
    !result <- bench
    when (expected /= result) $
      die $ "Expected " <> show expected <> ", but got " <> show result

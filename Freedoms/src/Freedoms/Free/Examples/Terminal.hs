{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Freedoms.Free.Examples.Terminal where

import Freedoms.Free.Church qualified as Free.Church
import Freedoms.Free.Data qualified as Free.Data
import Freedoms.Free.FinalClassy qualified as Free.FinalClassy
import Freedoms.Free.FinalReader qualified as Free.FinalReader

data Terminal a where
  Print :: String -> k -> Terminal k
  ReadLine :: (String -> k) -> Terminal k
  deriving (Functor)

interpretTerminal :: forall a. Terminal a -> IO a
interpretTerminal = \case
  Print a k -> putStrLn a >> pure k
  ReadLine k -> fmap k getLine

exampleFreeData :: IO ()
exampleFreeData = Free.Data.run interpretTerminal $ do
  print "echo:"
  msg <- readLine
  print msg
  where
    print s = Free.Data.free (Print s ())
    readLine = Free.Data.free (ReadLine id)

exampleFreeChurch :: IO ()
exampleFreeChurch = Free.Church.run interpretTerminal $ do
  print "echo:"
  msg <- readLine
  print msg
  where
    print s = Free.Church.free (Print s ())
    readLine = Free.Church.free (ReadLine id)

exampleFreeFinalReader :: IO ()
exampleFreeFinalReader = Free.FinalReader.run interpretTerminal $ do
  print "echo:"
  msg <- readLine
  print msg
  where
    print s = Free.FinalReader.free (Print s ())
    readLine = Free.FinalReader.free (ReadLine id)

exampleFreeFinalClassy :: IO ()
exampleFreeFinalClassy = Free.FinalClassy.run $ do
  print "echo:"
  msg <- readLine
  print msg
  where
    print s = Free.FinalClassy.free (Print s ())
    readLine = Free.FinalClassy.free (ReadLine id)

instance Free.FinalClassy.Interpreter Terminal IO where
  interpret = interpretTerminal

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Free.Example where

import Control.Applicative (liftA)
import Control.Monad (ap)
import Data.Kind (Type)
import Free.Church qualified
import Free.Data qualified
import Free.FinalReader qualified
import Free.FinalClassy qualified

-- An example functor
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

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE NoStarIsType #-}

module Free.Example where

import Control.Applicative (liftA)
import Control.Monad (ap)
import Data.Kind (Type)
import Free.Church qualified
import Free.Data qualified

-- A sample functor
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
  print s = Free.Data.lift (Print s (pure ()))
  readLine = Free.Data.lift (ReadLine pure)

exampleFreeChurch :: IO ()
exampleFreeChurch = Free.Church.run interpretTerminal $ do
  print "echo:"
  msg <- readLine
  print msg
 where
  print s = Free.Church.lift (Print s (pure ()))
  readLine = Free.Church.lift (ReadLine pure)

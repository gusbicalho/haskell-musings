{-# LANGUAGE
    FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , TypeSynonymInstances
#-}
module RealWorldHaskell.Ch15.WriterIO where

import RealWorldHaskell.Ch15.MonadHandle
import Control.Monad.Writer
import System.IO(IOMode(..))

data Event = Open FilePath IOMode
           | Put FilePath String
           | Close FilePath
           | GetContents FilePath
             deriving (Show)

newtype WriterIO a = W { runW :: Writer [Event] a }
  deriving (Functor, Applicative, Monad, MonadWriter [Event])

runWriterIO :: WriterIO a -> (a, [Event])
runWriterIO = runWriter . runW

instance MonadHandle FilePath WriterIO where
  openFile path mode = tell [Open path mode] >> return path
  hPutStr h s = tell [Put h s]
  hClose h = tell [Close h]
  hGetContents h = tell [GetContents h] >> return ""

-- >>> import RealWorldHaskell.Ch15.SafeHello
-- >>> runWriterIO $ safeHello "foo"
-- ((),[Open "foo" WriteMode,Put "foo" "hello world",Put "foo" "\n",Close "foo"])
--

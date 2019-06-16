{-# LANGUAGE
    FunctionalDependencies
#-}
module RealWorldHaskell.Ch15.MonadHandle
  ( MonadHandle(..)
  ) where

import qualified System.IO
import System.IO (IOMode(..))

class Monad m => MonadHandle h m | m -> h where
  openFile :: FilePath -> IOMode -> m h
  hPutStr :: h -> String -> m ()
  hClose :: h -> m ()
  hGetContents :: h -> m String

  hPutStrLn :: h -> String -> m ()
  hPutStrLn h s = hPutStr h s >> hPutStr h "\n"

instance MonadHandle System.IO.Handle IO where
  openFile = System.IO.openFile
  hPutStr = System.IO.hPutStr
  hClose = System.IO.hClose
  hGetContents = System.IO.hGetContents
  hPutStrLn = System.IO.hPutStrLn

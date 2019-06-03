module RealWorldHaskell.Ch08.GlobFile ( namesMatching ) where

import RealWorldHaskell.Ch08.Glob ( matchesGlob, isGlob )

import System.FilePath ( dropTrailingPathSeparator, splitFileName, (</>) )
import System.Directory ( doesFileExist, doesDirectoryExist, getCurrentDirectory
                        , getDirectoryContents )
import Control.Monad ( forM, filterM )
import Control.Exception (handle)
import Data.Either ( either )

namesMatching :: String -> IO [String]
namesMatching pat
  | not . isGlob $ pat =
      do exist <- doesNameExist pat
         return [pat | exist]
  | otherwise =
      case splitFileName pat of
        ("", baseName) -> do
          curDir <- getCurrentDirectory
          listMatches curDir baseName
        (dirPat, baseName) -> do
          dirs <- dirsMatching dirPat
          pathNames <- forM dirs $ filesMatching baseName
          return (concat pathNames)
  where dirsMatching dirPat =
          if isGlob dirPat
          then namesMatching (dropTrailingPathSeparator dirPat)
          else return [dirPat]
        filesMatching namePat dir =
          let listDir = if isGlob namePat
                        then (flip listMatches) namePat
                        else (flip listPlain) namePat
          in map (dir </>) <$> listDir dir

(||>) :: (Monad m) => Bool -> m Bool -> m Bool
a ||> mb = if a then return a else mb

(<||>) :: (Monad m) => m Bool -> m Bool -> m Bool
ma <||> mb = ma >>= (||> mb)
infixr 2 <||>

doesNameExist :: FilePath -> IO Bool
doesNameExist pat = doesFileExist pat <||> doesDirectoryExist pat

listMatches :: FilePath -> String -> IO [String]
listMatches "" name = do dir <- getCurrentDirectory
                         listMatches dir name
listMatches dir pat =
  handle (const $ return [] :: IOError -> IO [String]) $ do
    names <- getDirectoryContents dir
    filterM matchGlobCatchingLeftAsIOError . filter fileFilter $ names
  where fileFilter = if isHidden pat then isHidden else not . isHidden
        matchGlobCatchingLeftAsIOError :: String -> IO Bool
        matchGlobCatchingLeftAsIOError = either (ioError . userError) return . matchesGlob pat

isHidden :: String -> Bool
isHidden ('.':_) = True
isHidden _       = False

listPlain :: FilePath -> String -> IO [String]
listPlain dir ""    = do exists <- doesDirectoryExist dir
                         return [dir | exists]
listPlain dir fname = do exists <- doesNameExist path
                         return [path | exists]
  where path = dir </> fname

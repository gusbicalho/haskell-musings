{-# LANGUAGE NamedFieldPuns #-}
module RealWorldHaskell.Ch09.ControlledTraversal
  ( traverseContents
  , getUsefulContents
  , Info(..)
  , getInfo
  , isDirectory
  ) where

import System.FilePath ( (</>) )
import System.Directory ( Permissions(..)
                        , doesDirectoryExist
                        , getDirectoryContents
                        , getPermissions
                        , getFileSize
                        , getModificationTime
                        )
import Data.Time.Clock ( UTCTime )
import Control.Monad ( mapM, forM, join )
import Control.Exception ( handle )

data Info = Info { infoPath    :: FilePath          -- path to directory entry
                 , infoPerms   :: Maybe Permissions -- permissions
                 , infoSize    :: Maybe Integer     -- file size (Nothing if not file)
                 , infoModTime :: Maybe UTCTime     -- last modified
                 } deriving (Eq, Ord, Show)

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle doNothing (Just <$> act)
  where doNothing :: IOError -> IO (Maybe a)
        doNothing = const $ return Nothing

getInfo :: FilePath -> IO Info
getInfo path = Info path <$> maybeIO (getPermissions path)
                         <*> (join <$> maybeIO fileSize)
                         <*> maybeIO (getModificationTime path)
  where fileSize = do isDir <- doesDirectoryExist path
                      if isDir
                        then return Nothing
                        else Just <$> getFileSize path

traverseContents :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverseContents order root = do
  childrenNames <- getUsefulContents root
  let childrenPaths = map (root </>) childrenNames
  infos <- mapM getInfo ( root : childrenPaths )
  fmap concat . forM (order infos) $ \info ->
    if isDirectory info && infoPath info /= root
      then traverseContents order (infoPath info)
      else return [info]

isDirectory :: Info -> Bool
isDirectory Info { infoPerms } = maybe False searchable infoPerms

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
  names <- getDirectoryContents path
  return (filter (`notElem` [".", ".."]) names)

{-# LANGUAGE TupleSections, RecordWildCards #-}
module RecursiveContents ( getRecursiveContents
                         , simpleFind
                         , betterFind
                         , Info(..)
                         , justSizeP
                         , constP
                         , liftP
                         , liftP2
                         , mapP
                         , liftPath
                         , (==?), (<?), (>?)
                         , (?&&?), (?||?)
                         ) where

import Data.Time.Clock
import System.Directory ( Permissions
                        , doesDirectoryExist
                        , getDirectoryContents
                        , getPermissions
                        , getFileSize
                        , getModificationTime
                        )
import System.FilePath
import Control.Monad
import Control.Exception (handle)
import Data.List ( isSuffixOf )

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topDir = do
    contents <- getDirectoryContents topDir
    let children = map (topDir </>) . filter (`notElem` specialNames) $ contents
    recursiveDescendants <- mapM (fileSpec >=> descendants) children
    return $ topDir : concat recursiveDescendants
  where specialNames = [".", ".."]
        fileSpec name = (name,) <$> doesDirectoryExist name
        descendants (name, False) = return [name]
        descendants (name, True)  = getRecursiveContents name

simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind p path = filter p <$> getRecursiveContents path

-- >>> simpleFind ((== ".hs") . takeExtension) "src"
-- ["src/BinaryTree.hs","src/FiniteAutomata.hs","src/HTree.hs","src/Lens.hs","src/RealWorldHaskell/Ch05/Prettify.hs","src/RealWorldHaskell/Ch05/PrettyJSON.hs","src/RealWorldHaskell/Ch05/PutJSON.hs","src/RealWorldHaskell/Ch05/SimpleJSON.hs","src/RealWorldHaskell/Ch08/Glob.hs","src/RealWorldHaskell/Ch08/GlobFile.hs","src/RealWorldHaskell/Ch09/RecursiveContents.hs","src/RealWorldHaskell/ch01/WC.hs","src/RealWorldHaskell/ch03.hs","src/RealWorldHaskell/ch04.hs","src/TypeLevel/List.hs","src/TypeLevel/Reflection.hs","src/TypeLevel/Set.hs","src/scratch.hs","src/sorter.hs"]
--

data Info = Info { infoPath :: FilePath      -- path to directory entry
                 , infoPerms :: Permissions  -- permissions
                 , infoSize :: Maybe Integer -- file size (Nothing if not file)
                 , infoModTime :: UTCTime    -- last modified
                 } deriving (Eq, Ord, Show)

type InfoP a = Info -> a

type Predicate = InfoP Bool

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind f root = getRecursiveContents root >>= filterM check
  where
    check :: FilePath -> IO Bool
    check path = f <$> getInfo path

getInfo :: FilePath -> IO Info
getInfo path = Info path <$> getPermissions path
                         <*> getFileSize' path
                         <*> getModificationTime path

getFileSize' :: FilePath -> IO (Maybe Integer)
getFileSize' path = handle (const $ return Nothing :: IOError -> IO (Maybe a)) $ do
                      isDirectory <- doesDirectoryExist path
                      if isDirectory
                        then return Nothing
                        else Just <$> getFileSize path

justSizeP :: InfoP Integer
justSizeP Info { infoSize = Just infoSize } = infoSize
justSizeP _                                 = -1

constP :: a -> InfoP a
constP a _ = a

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 op infoA infoB info = infoA info `op` infoB info

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP op info b = (liftP2 op) info (constP b)

liftPath :: (FilePath -> a) -> InfoP a
liftPath f Info { .. } = f infoPath

mapP :: (a -> b) -> InfoP a -> InfoP b
mapP f infoP info = f $ infoP info

(==?) :: Eq a => InfoP a -> a -> InfoP Bool
(==?) = liftP (==)
infix 4 ==?

(<?) :: Ord a => InfoP a -> a -> InfoP Bool
(<?) = liftP (<)
infix 4 <?

(>?) :: Ord a => InfoP a -> a -> InfoP Bool
(>?) = liftP (>)
infix 4 >?

(?&&?) :: InfoP Bool -> InfoP Bool -> InfoP Bool
(?&&?) = liftP2 (&&)
infixr 3 ?&&?

(?||?) = liftP2 (||)
infixr 2 ?||?

{-
>>> betterFind (liftPath takeExtension ==? ".hs" ?&&? justSizeP >? 3096) "."
["./src/HTree.hs","./src/Lens.hs","./src/RealWorldHaskell/Ch09/RecursiveContents.hs"]

-}

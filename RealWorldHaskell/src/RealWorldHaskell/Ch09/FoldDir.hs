{-# LANGUAGE NamedFieldPuns #-}
module RealWorldHaskell.Ch09.FoldDir
  ( foldDir
  , filesOnly
  , directoriesOnly
  , identityI
  , collectI
  , andThenI
  , whenI
  ) where

import RealWorldHaskell.Ch09.ControlledTraversal ( Info(..)
                                                 , getInfo
                                                 , getUsefulContents
                                                 , isDirectory
                                                 )
import System.FilePath ( (</>) )

data Iterate seed = Done     { unwrap :: seed }
                  | Skip     { unwrap :: seed }
                  | Continue { unwrap :: seed }
                    deriving (Show)

type Iterator seed = seed -> Info -> Iterate seed

foldDir :: Iterator a -> a -> FilePath -> IO a
foldDir iter initSeed root = unwrap <$> fold initSeed root
  where
    fold seed path = getUsefulContents path >>= walk seed path
    walk seed _    [] = return $ Continue seed
    walk seed path (name : names) = do
      let subpath = path </> name
      info <- getInfo subpath
      case iter seed info of
        done@(Done _)    -> return done
        Skip     newSeed -> walk newSeed path names
        Continue newSeed
          | isDirectory info -> do
            next <- fold newSeed subpath
            case next of
              done@(Done _) -> return done
              _             -> walk (unwrap next) path names
          | otherwise -> walk newSeed path names

identityI :: Iterator a
identityI seed _ = Continue seed

collectI :: (a -> Info -> a) -> Iterator a
collectI f seed info = Continue $ f seed info

whenI :: (Info -> Bool) -> (a -> Info -> a) -> (a -> Info -> a)
whenI p transform v info | p info    = transform v info
                         | otherwise = v

andThenI :: Iterator a -> (a -> Iterator b) -> Iterator (a, b)
andThenI iterA iterBFactory (seedA, seedB) info =
  case iterA seedA info of
    Done     seedA' -> Done (seedA', seedB)
    Skip     seedA' -> Skip (seedA', seedB)
    Continue seedA' ->
      case iterBFactory seedA' seedB info of
        Done     seedB' -> Done     (seedA', seedB')
        Skip     seedB' -> Skip     (seedA', seedB')
        Continue seedB' -> Continue (seedA', seedB')

filesOnly :: Iterator [Info]
filesOnly = collectI (whenI (not . isDirectory) (flip (:)))

directoriesOnly :: Iterator [Info]
directoriesOnly = collectI (whenI isDirectory (flip (:)))

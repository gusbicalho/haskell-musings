{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Freedoms.Freer.Examples.Terminal where

import Control.Monad.Trans.Reader (ReaderT (ReaderT, runReaderT))
import Freedoms.Freer.Church qualified as Freer.Church
import Freedoms.Freer.Common (Interpret, RunCont)
import Freedoms.Freer.Data qualified as Freer.Data
import Freedoms.Freer.FinalClassy qualified as Freer.FinalClassy
import Freedoms.Freer.FinalReader qualified as Freer.FinalReader

data Terminal free a where
  Print :: String -> Terminal free ()
  ReadLine :: Terminal free String
  Prompt :: String -> free a -> Terminal free a

interpretTerminal ::
  forall n.
  RunCont n Terminal ->
  Interpret n Terminal IO
interpretTerminal runCont = flip runReaderT "" . go
  where
    go :: Interpret n Terminal (ReaderT String IO)
    go = \case
      Print a -> ReaderT $ \_ -> putStrLn a
      ReadLine -> ReaderT $ \p -> do
        putStr p
        getLine
      Prompt prompt k -> ReaderT $ \p ->
        runReaderT
          (runCont go k)
          (p <> prompt)

exampleFreerData :: IO ()
exampleFreerData = Freer.Data.run interpretTerminal $ do
  print "echo:"
  msg <- prompt "wat> " $ do
    readLine
  print msg
  where
    print s = Freer.Data.freer (Print s)
    readLine = Freer.Data.freer ReadLine
    prompt p m = Freer.Data.freer (Prompt p m)

exampleFreerChurch :: IO ()
exampleFreerChurch = Freer.Church.run interpretTerminal $ do
  print "echo:"
  msg <- prompt "wat> " $ do
    readLine
  print msg
  where
    print s = Freer.Church.freer (Print s)
    readLine = Freer.Church.freer ReadLine
    prompt p m = Freer.Church.freer (Prompt p m)

exampleFreerFinalReader :: IO ()
exampleFreerFinalReader = Freer.FinalReader.run interpretTerminal $ do
  print "echo:"
  msg <- prompt "wat> " $ do
    readLine
  print msg
  where
    print s = Freer.FinalReader.freer (Print s)
    readLine = Freer.FinalReader.freer ReadLine
    prompt p m = Freer.FinalReader.freer (Prompt p m)

exampleFreerFinalClassy :: IO ()
exampleFreerFinalClassy = Freer.FinalClassy.run $ do
  print "echo:"
  msg <- prompt "wat> " $ do
    readLine
  print msg
  where
    print s = Freer.FinalClassy.freer (Print s)
    readLine = Freer.FinalClassy.freer ReadLine
    prompt p m = Freer.FinalClassy.freer (Prompt p m)

instance Freer.FinalClassy.Interpreter Terminal IO where
  interpret = flip runReaderT "" . Freer.FinalClassy.interpret

instance Freer.FinalClassy.Interpreter Terminal (ReaderT String IO) where
  interpret = \case
    Print a -> ReaderT $ \_ -> putStrLn a
    ReadLine -> ReaderT $ \p -> do
      putStr p
      getLine
    Prompt prompt k -> ReaderT $ \p ->
      runReaderT
        (Freer.FinalClassy.run k)
        (p <> prompt)

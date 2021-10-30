{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Bench.Counter where

import Control.Monad (replicateM_)
import Control.Monad.Trans.Reader (ReaderT (ReaderT, runReaderT))
import Control.Monad.Trans.Reader qualified as ReaderT
import Control.Monad.Trans.State.Strict (State)
import Control.Monad.Trans.State.Strict qualified as State
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Kind (Type)
import Data.Word (Word8)
import Freedoms.Free.Church qualified as Free.Church
import Freedoms.Free.Data qualified as Free.Data
import Freedoms.Free.FinalClassy qualified as Free.FinalClassy
import Freedoms.Free.FinalReader qualified as Free.FinalReader
import Freedoms.Freer.Church qualified as Freer.Church
import Freedoms.Freer.Common (Interpret, RunCont)
import Freedoms.Freer.Data qualified as Freer.Data
import Freedoms.Freer.FinalClassy qualified as Freer.FinalClassy
import Freedoms.Freer.FinalReader qualified as Freer.FinalReader

iterations :: Int
iterations = 10_000

benchIOFreeChurch :: IO Word8
benchIOFreeChurch = do
  ioRef <- IORef.newIORef @Word8 0
  Free.Church.run (interpretCounterFInIO ioRef) $ do
    replicateM_ iterations $ do
      inc
      pure ()
    inc
 where
  inc = Free.Church.free (Inc' id)

benchStateFreeChurch :: Word8
benchStateFreeChurch =
  flip State.execState 0 $
    Free.Church.run interpretCounterFInState $ do
      replicateM_ iterations $ do
        inc
        pure ()
      inc
 where
  inc = Free.Church.free (Inc' id)

benchIOFreeData :: IO Word8
benchIOFreeData = do
  ioRef <- IORef.newIORef @Word8 0
  Free.Data.run (interpretCounterFInIO ioRef) $ do
    replicateM_ iterations $ do
      inc
      pure ()
    inc
 where
  inc = Free.Data.free (Inc' id)

benchStateFreeData :: Word8
benchStateFreeData =
  flip State.execState 0 $
    Free.Data.run interpretCounterFInState $ do
      replicateM_ iterations $ do
        inc
        pure ()
      inc
 where
  inc = Free.Data.free (Inc' id)

benchIOFreeFinalReader :: IO Word8
benchIOFreeFinalReader = do
  ioRef <- IORef.newIORef @Word8 0
  Free.FinalReader.run (interpretCounterFInIO ioRef) $ do
    replicateM_ iterations $ do
      inc
      pure ()
    inc
 where
  inc = Free.FinalReader.free (Inc' id)

benchStateFreeFinalReader :: Word8
benchStateFreeFinalReader =
  flip State.execState 0 $
    Free.FinalReader.run interpretCounterFInState $ do
      replicateM_ iterations $ do
        inc
        pure ()
      inc
 where
  inc = Free.FinalReader.free (Inc' id)

benchIOFreeFinalClassy :: IO Word8
benchIOFreeFinalClassy = do
  ioRef <- IORef.newIORef @Word8 0
  runReaderT go ioRef
 where
  go = Free.FinalClassy.run $ do
    replicateM_ iterations $ do
      inc
      pure ()
    inc
  inc = Free.FinalClassy.free (Inc' id)

benchStateFreeFinalClassy :: Word8
benchStateFreeFinalClassy =
  flip State.execState 0 $
    Free.FinalClassy.run $ do
      replicateM_ iterations $ do
        inc
        pure ()
      inc
 where
  inc = Free.FinalClassy.free (Inc' id)

benchIOFreerChurch :: IO Word8
benchIOFreerChurch = do
  ioRef <- IORef.newIORef @Word8 0
  Freer.Church.run (\_ -> interpretCounterInIO ioRef) $ do
    replicateM_ iterations $ do
      inc
      pure ()
    inc
 where
  inc = Freer.Church.freer Inc

benchStateFreerChurch :: Word8
benchStateFreerChurch =
  flip State.execState 0 $
    Freer.Church.run (\_ -> interpretCounterInState) $ do
      replicateM_ iterations $ do
        inc
        pure ()
      inc
 where
  inc = Freer.Church.freer Inc

benchIOFreerData :: IO Word8
benchIOFreerData = do
  ioRef <- IORef.newIORef @Word8 0
  Freer.Data.run (\_ -> interpretCounterInIO ioRef) $ do
    replicateM_ iterations $ do
      inc
      pure ()
    inc
 where
  inc = Freer.Data.freer Inc

benchStateFreerData :: Word8
benchStateFreerData =
  flip State.execState 0 $
    Freer.Data.run (\_ -> interpretCounterInState) $ do
      replicateM_ iterations $ do
        inc
        pure ()
      inc
 where
  inc = Freer.Data.freer Inc

benchIOFreerFinalReader :: IO Word8
benchIOFreerFinalReader = do
  ioRef <- IORef.newIORef @Word8 0
  Freer.FinalReader.run (\_ -> interpretCounterInIO ioRef) $ do
    replicateM_ iterations $ do
      inc
      pure ()
    inc
 where
  inc = Freer.FinalReader.freer Inc

benchStateFreerFinalReader :: Word8
benchStateFreerFinalReader =
  flip State.execState 0 $
    Freer.FinalReader.run (\_ -> interpretCounterInState) $ do
      replicateM_ iterations $ do
        inc
        pure ()
      inc
 where
  inc = Freer.FinalReader.freer Inc

benchIOFreerFinalClassy :: IO Word8
benchIOFreerFinalClassy = do
  ioRef <- IORef.newIORef @Word8 0
  runReaderT go ioRef
 where
  go = Freer.FinalClassy.run $ do
    replicateM_ iterations $ do
      inc
      pure ()
    inc
  inc = Freer.FinalClassy.freer Inc

benchStateFreerFinalClassy :: Word8
benchStateFreerFinalClassy =
  flip State.execState 0 $
    Freer.FinalClassy.run $ do
      replicateM_ iterations $ do
        inc
        pure ()
      inc
 where
  inc = Freer.FinalClassy.freer Inc

-- Effect and interpreters

data Counter (free :: Type -> Type) a where
  Inc :: Counter free Word8

data CounterF a where
  Inc' :: (Word8 -> k) -> CounterF k
  deriving (Functor)

interpretCounterFInIO :: IORef Word8 -> forall a. CounterF a -> IO a
interpretCounterFInIO ioRef = \case
  Inc' k -> k <$> IORef.atomicModifyIORef' ioRef (dup . inc)

instance Free.FinalClassy.Interpreter CounterF (ReaderT (IORef Word8) IO) where
  interpret c = ReaderT $ case c of
    Inc' k -> \ioRef -> k <$> IORef.atomicModifyIORef' ioRef (dup . inc)

interpretCounterFInState :: forall a. CounterF a -> State Word8 a
interpretCounterFInState = \case
  Inc' k -> fmap k $ State.modify' inc >> State.get

instance Free.FinalClassy.Interpreter CounterF (State Word8) where
  interpret = interpretCounterFInState

interpretCounterInIO ::
  IORef Word8 ->
  forall n.
  Interpret n Counter IO
interpretCounterInIO ioRef = \case
  Inc -> IORef.atomicModifyIORef' ioRef (dup . inc)

interpretCounterInState :: forall n. Interpret n Counter (State Word8)
interpretCounterInState = \case
  Inc -> State.modify' inc >> State.get

instance Freer.FinalClassy.Interpreter Counter (ReaderT (IORef Word8) IO) where
  interpret c = ReaderT $ case c of
    Inc -> \ioRef -> IORef.atomicModifyIORef' ioRef (dup . inc)

instance Freer.FinalClassy.Interpreter Counter (State Word8) where
  interpret = interpretCounterInState

dup :: x -> (x, x)
dup x = (x, x)

inc :: Word8 -> Word8
inc = (+ 1)

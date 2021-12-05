{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}

module Freedoms.FreerExtensible.FinalReader (Freer, freer, run) where

import Control.Applicative (liftA)
import Control.Monad (ap)
import Control.Monad.Trans.Reader (ReaderT (ReaderT, runReaderT))
import Data.Kind (Constraint, Type)

type Effect = ((Type -> Type) -> Type -> Type)

type SumF :: [Effect] -> (Type -> Type) -> Type -> Type
data SumF effects inner a where
  L :: forall effect moreEffects inner a. effect inner a -> SumF (effect ': moreEffects) inner a
  R :: forall effect moreEffects inner a. SumF moreEffects inner a -> SumF (effect ': moreEffects) inner a

type Sum effects a = SumF effects (Freer effects) a

type Inject :: Effect -> [Effect] -> Constraint
class Inject effect effects where
  inject :: forall inner a. effect inner a -> SumF effects inner a

instance
  {-# OVERLAPPING #-}
  (inner ~ Freer moreEffects) =>
  Inject effect (effect ': moreEffects)
  where
  {-# INLINE inject #-}
  inject = L

instance
  {-# OVERLAPPABLE #-}
  Inject effect moreEffects =>
  Inject effect (otherEffect ': moreEffects)
  where
  {-# INLINE inject #-}
  inject = R . inject

newtype Interpreted effects m a where
  Interpreted :: {runInterpreted :: (forall x. Sum effects x -> m x) -> m a} -> Interpreted effects m a
  deriving (Functor, Applicative, Monad) via (ReaderT (forall x. Sum effects x -> m x) m)

type Freer :: [Effect] -> (Type -> Type)
newtype Freer effects a where
  Freer :: {runMF :: forall m. Monad m => Interpreted effects m a} -> Freer effects a

instance Functor (Freer f) where
  {-# INLINE fmap #-}
  fmap = liftA

instance Applicative (Freer f) where
  {-# INLINE pure #-}
  pure a = Freer (pure a)
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Monad (Freer f) where
  {-# INLINE (>>=) #-}
  Freer ma >>= mkMb = Freer $ do
    a <- ma
    runMF $ mkMb a

{-# INLINE freer #-}
freer :: Inject effect effects => effect (Freer effects) a -> Freer effects a
freer fa = let !fa' = inject fa in Freer $ Interpreted (\f -> f fa')

type Interpreter effects m = forall x. Sum effects x -> m x
type RunFreer effects m = forall b. Interpreter effects m -> Freer effects b -> m b

{-# INLINE run #-}
run ::
  forall effects m a.
  Monad m =>
  (RunFreer effects m -> Interpreter effects m) ->
  Freer effects a ->
  m a
run mkInterpret = runIt (mkInterpret runIt)
 where
  {-# INLINE runIt #-}
  runIt :: RunFreer effects m
  runIt interpret ma =
    runInterpreted (runMF ma) interpret

-- EXAMPLE

type StdErr :: Effect
data StdErr free a where
  PrintErr :: String -> StdErr free ()

printErr :: Inject StdErr effects => String -> Freer effects ()
printErr = freer . PrintErr

type Terminal :: Effect
data Terminal free a where
  Print :: String -> Terminal free ()
  ReadLine :: Terminal free String
  Prompt :: String -> free a -> Terminal free a

print' :: Inject Terminal effects => String -> Freer effects ()
print' msg = freer $ Print msg

readLine :: Inject Terminal effects => Freer effects String
readLine = freer ReadLine

prompt :: Inject Terminal effects => String -> Freer effects a -> Freer effects a
prompt s action = freer $ Prompt s action

test :: (Inject Terminal effects, Inject StdErr effects) => Freer effects ()
test = do
  print' "echo:"
  msg <- prompt "wat> " $ do
    msg <- readLine
    printErr "ok."
    pure msg
  print' msg

runTest :: IO ()
runTest = flip runReaderT "" $ run mkInterpret test
 where
  mkInterpret :: RunFreer [StdErr, Terminal] (ReaderT String IO) -> Interpreter [StdErr, Terminal] (ReaderT String IO)
  mkInterpret run =
    let interpret :: Interpreter [StdErr, Terminal] (ReaderT String IO)
        interpret = \case
          L (PrintErr msg) -> ReaderT $ \_ -> print $ "Err: " <> msg
          R (L terminal) -> case terminal of
            Print a -> ReaderT $ \_ -> putStrLn a
            ReadLine -> ReaderT $ \p -> do
              putStr p
              getLine
            Prompt prompt k -> ReaderT $ \p ->
              runReaderT
                (run interpret k)
                (p <> prompt)
          R (R n) -> case n of {}
    in interpret

module Bidirectional.ReportTypeErrors where

import Control.Monad.Trans.Class qualified as Trans
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except qualified as Except
import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Trans.State.Strict qualified as StateT
import Data.List qualified as List

class Monad tc => ReportTypeErrors tc where
  reportTypeError :: String -> tc a
  catchTypeError :: tc a -> (String -> tc a) -> tc a

typeError :: ReportTypeErrors tc => [String] -> tc a
typeError = reportTypeError . List.intercalate "\n"

ind :: [String] -> String
ind = List.intercalate "\n" . fmap ("  " <>)

ind1 :: Show a => a -> String
ind1 x = ind [show x]

-- Instances

instance Monad m => ReportTypeErrors (ExceptT String m) where
  reportTypeError = Except.throwE
  catchTypeError = Except.catchE

instance ReportTypeErrors m => ReportTypeErrors (StateT s m) where
  reportTypeError = Trans.lift . reportTypeError
  catchTypeError = StateT.liftCatch catchTypeError

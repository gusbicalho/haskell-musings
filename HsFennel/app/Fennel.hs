{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Fennel (register, repl) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Data.Functor (void)
import HsLua (Lua)
import HsLua qualified as Lua

fennelLua :: ByteString
fennelLua = $(embedFile "resources/fennel.lua")

register :: Lua ()
register = do
  Lua.preloadhs "fennel" $ Lua.NumResults 1 <$ Lua.dostring fennelLua
  void $
    Lua.dostring
      "local fennel = require('fennel');\
      \table.insert(package.searchers, fennel.searcher)"

repl :: Lua Lua.Status
repl = do
  Lua.dostring
    "local fennel = require('fennel');\
    \debug.traceback = fennel.traceback;\
    \fennel.repl();"

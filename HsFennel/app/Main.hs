{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Version qualified as Version
import Fennel qualified
import HsLua (Lua, (###), (#?), (<#>), (=#>))
import HsLua qualified as Lua

main :: IO ()
main = do
  putStrLn . unlines $
    [ "This is a Fennel REPL running on a Haskell program!"
    , "Try running:"
    , "  ,help"
    , "  (local hs (require :hs-fns))"
    , "  (hs.factorial 5)"
    ]
  print =<< Lua.run do
    Lua.openlibs
    registerHaskellModule
    Fennel.register
    Fennel.repl

-- mostly copied from https://git.sr.ht/~jack/hslua-fennel-demo/tree/master/item/app/Main.hs

{- | The 'L.DocumentedFunction' machinery is from "hslua-packaging";
 we can provide to Lua any function returning @'LuaE' e a@, so long
 as we can provide a 'Peeker' for each argument and a 'Pusher' for
 each result.
-}
factorial :: Lua.DocumentedFunction e
factorial =
  Lua.defun "factorial"
    ### Lua.liftPure (\n -> product @_ @Integer [1 .. n])
    <#> Lua.integralParam "n" "input number"
    =#> Lua.integralResult "factorial of n"
    #? "Computes the factorial of an integer."
    `Lua.since` Version.makeVersion [1, 0, 0]

{- | Also using "hslua-packaging", this registers our
 (single-function) module into Lua's @package.preload@ table,
 setting things up such that the first time
 @require('my-haskell-module')@ is called, the module will be
 assembled, stored in @package.loaded['my-haskell-module']@ and
 returned.

 This lazy loading can help with the startup time of larger programs.

 /See:/ http://www.lua.org/manual/5.4/manual.html#pdf-require
-}
registerHaskellModule :: Lua ()
registerHaskellModule =
  Lua.preloadModule
    Lua.Module
      { Lua.moduleName = "hs-fns"
      , Lua.moduleDescription = "Functions from Haskell"
      , Lua.moduleFields = []
      , Lua.moduleFunctions = [factorial]
      , Lua.moduleOperations = []
      }

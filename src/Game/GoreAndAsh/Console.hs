{-|
Module      : Game.GoreAndAsh.Console
Description : Module that contains console interaction API for Gore&Ash
Copyright   : (c) Anton Gushcha, 2015-2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

The module contains an API for Gore&Ash console interaction module. The module
doesn't depends on others core modules and could be place in any place in game monad stack.

Example of embedding:

@
import Game.GoreAndAsh.Core
import Game.GoreAndAsh.Console

type AppMonad t a = ConsoleT t (GameMonad t) a

app :: ConsoleMonad t m => m ()
app = do
  e <- consolePrompt' "Enter a string: "
  consolePutLn $ fmap ("You entered: " ++) e

main :: IO ()
main = runSpiderHost $ hostApp $ runModule () (app :: AppMonad Spider ())
@

For example with newtype see `examples/Example02.hs`.
-}
module Game.GoreAndAsh.Console(
    ConsoleEventStopper
  , ConsoleMonad(..)
  , ConsoleT
  , consolePutLn
  , consoleInputForever'
  , consoleCharInputForever'
  , consolePromptForever'
  , consolePromptValidate
  ) where

import Game.GoreAndAsh.Console.API
import Game.GoreAndAsh.Console.Module

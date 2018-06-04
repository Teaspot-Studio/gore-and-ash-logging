{-# LANGUAGE RecursiveDo #-}
module Main where

import Game.GoreAndAsh.Core
import Game.GoreAndAsh.Console
import Data.Proxy
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.Catch

type AppMonad = GMSpider

app :: MonadGame t m => m ()
app = mdo
  e <- consolePromptForever' "Enter a string: " (void printedE)
  printedE <- consolePutLn $ fmap ("You entered: " ++) e
  pure ()

main :: IO ()
main = runGM (app :: AppMonad ())

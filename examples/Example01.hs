module Main where

import Game.GoreAndAsh.Core
import Game.GoreAndAsh.Logging
import Data.Proxy
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.Catch

type AppMonad = LoggingT Spider GMSpider

app :: LoggingMonad t m => m ()
app = mapM_ (logMsgLnM LogInfo) $ fmap showl [1 .. 10 :: Int]

main :: IO ()
main = runGM $ runLoggerT (app :: AppMonad ())

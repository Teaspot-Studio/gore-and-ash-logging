{-|
Module      : Game.GoreAndAsh.Logging
Description : Module that contains console interaction API for Gore&Ash
Copyright   : (c) Anton Gushcha, 2015-2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

The module contains an API for Gore&Ash logging module. The module doesn't depends on
others core modules and could be place in any place in game monad stack.

Example of embedding:

@
import Game.GoreAndAsh.Core
import Game.GoreAndAsh.Logging

type AppMonad t a = LoggingT t (GameMonad t) a

app :: LoggingMonad t m => m ()
app = mapM_ (logMsgLnM LogInfo) $ fmap showl [1 .. 10 :: Int]

main :: IO ()
main = runSpiderHost $ hostApp $ runModule () (app :: AppMonad Spider ())
@

For example with newtype see `examples/Example01.hs`.
-}
module Game.GoreAndAsh.Logging(
  -- * Low-level API
    LoggingEnv
  , LoggingT
  , LoggingLevel(..)
  , LoggingSink(..)
  , LoggingMonad(..)
  , LogStr
  , toLogStr
  , showl
  -- * Reactive API
  , logDyn
  , logDynLn
  -- * Shortcuts
  -- ** Behavior
  , logDebug
  , logInfo
  , logWarn
  , logError
  -- ** Dynamic
  , logDebugDyn
  , logInfoDyn
  , logWarnDyn
  , logErrorDyn
  -- ** Event
  , logDebugE
  , logInfoE
  , logWarnE
  , logErrorE
  , logEither
  , logEitherWarn
  , logEitherError
  -- ** Verbose logging
  , logVerboseM
  , logVerbose
  , logVerboseDyn
  , logVerboseE
  ) where

import Game.GoreAndAsh.Logging.API
import Game.GoreAndAsh.Logging.Module
import Game.GoreAndAsh.Logging.State

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Game.GoreAndAsh.Logging.Module
Description : Monad transformer for logging module
Copyright   : (c) Anton Gushcha, 2015-2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

Contains description of logging monad transformer and instance for
'GameModule' class.
-}
module Game.GoreAndAsh.Logging.Module(
    LoggingT(..)
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Error.Class
import Control.Monad.Fix
import Control.Monad.State.Strict
import Control.Monad.Trans.Resource
import Data.Proxy

import Game.GoreAndAsh
import Game.GoreAndAsh.Logging.State

-- | Monad transformer of logging core module.
--
-- [@s@] - State of next core module in modules chain;
--
-- [@m@] - Next monad in modules monad stack;
--
-- [@a@] - Type of result value;
--
-- How to embed module:
--
-- @
-- type AppStack = ModuleStack [LoggingT, ... other modules ... ] IO
--
-- newtype AppMonad a = AppMonad (AppStack a)
--   deriving (Functor, Applicative, Monad, MonadFix, MonadIO, LoggingMonad)
-- @
--
-- The module is pure within first phase (see 'ModuleStack' docs) and could be used
-- with 'Identity' end monad.
newtype LoggingT m a = LoggingT { runLoggingT :: StateT LoggingState m a }
  deriving (Functor, Applicative, Monad, MonadState LoggingState, MonadFix
    , MonadTrans, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadError e
    , MonadSample t, MonadHold t)

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (LoggingT m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger initializer = lift $ newFanEventWithTrigger initializer

instance MonadSubscribeEvent t m => MonadSubscribeEvent t (LoggingT m) where
  subscribeEvent = lift . subscribeEvent

instance MonadAppHost t m => MonadAppHost t (LoggingT m) where
  getFireAsync = lift getFireAsync
  getRunAppHost = do
    runner <- LoggingT getRunAppHost
    return $ \m -> runner $ runLoggingT m
  performPostBuild_ = lift . performPostBuild_
  liftHostFrame = lift . liftHostFrame

instance MonadBase IO m => MonadBase IO (LoggingT m) where
  liftBase = LoggingT . liftBase

instance MonadResource m => MonadResource (LoggingT m) where
  liftResourceT = LoggingT . liftResourceT

instance (MonadIO (HostFrame t), GameModule t m) => GameModule t (LoggingT m) where
  type ModuleOptions t (LoggingT m) = ModuleOptions t m
  runModule opts (LoggingT m) = do
    s <- emptyLoggingState
    runModule opts $ evalStateT m s
  withModule t _ = withModule t (Proxy :: Proxy m)
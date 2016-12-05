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
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Proxy

import Game.GoreAndAsh
import Game.GoreAndAsh.Logging.State

-- | Monad transformer of logging core module.
--
-- [@t@] - FRP engine implementation, safely can be ignored;
--
-- [@m@] - Next monad in modules monad stack;
--
-- [@a@] - Type of result value;
--
-- How to embed module:
--
-- @
-- type AppMonad t a = LoggingT t (GameMonad t)
--
-- app :: LoggingMonad t m => m ()
-- app = mapM_ (logMsgLnM LogInfo) $ fmap showl [1 .. 10 :: Int]
-- @
--
-- See `examples/Example01.hs` for a full example.
newtype LoggingT t m a = LoggingT { runLoggingT :: ReaderT (LoggingEnv t) m a }
  deriving (Functor, Applicative, Monad, MonadReader (LoggingEnv t), MonadFix
    , MonadIO, MonadThrow, MonadCatch, MonadMask, MonadSample t, MonadHold t)

instance MonadTrans (LoggingT t) where
  lift = LoggingT . lift

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (LoggingT t m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger initializer = lift $ newFanEventWithTrigger initializer

instance MonadSubscribeEvent t m => MonadSubscribeEvent t (LoggingT t m) where
  subscribeEvent = lift . subscribeEvent

instance MonadAppHost t m => MonadAppHost t (LoggingT t m) where
  getFireAsync = lift getFireAsync
  getRunAppHost = do
    runner <- LoggingT getRunAppHost
    return $ \m -> runner $ runLoggingT m
  performPostBuild_ = lift . performPostBuild_
  liftHostFrame = lift . liftHostFrame

instance MonadBase IO m => MonadBase IO (LoggingT t m) where
  liftBase = LoggingT . liftBase

instance MonadResource m => MonadResource (LoggingT t m) where
  liftResourceT = LoggingT . liftResourceT

instance (MonadIO (HostFrame t), GameModule t m) => GameModule t (LoggingT t m) where
  type ModuleOptions t (LoggingT t m) = ModuleOptions t m
  runModule opts (LoggingT m) = do
    s <- emptyLoggingEnv
    runModule opts $ runReaderT m s
  withModule t _ = withModule t (Proxy :: Proxy m)

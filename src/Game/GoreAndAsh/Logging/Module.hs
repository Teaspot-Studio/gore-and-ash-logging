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
import Control.Monad.Extra (whenJust, whenM)
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.Monoid
import Data.Proxy
import System.Log.FastLogger

import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS

import Game.GoreAndAsh
import Game.GoreAndAsh.Logging.API
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

instance MonadTransControl (LoggingT t) where
  type StT (LoggingT t) a = StT (ReaderT (LoggingEnv t)) a
  liftWith = defaultLiftWith LoggingT runLoggingT
  restoreT = defaultRestoreT LoggingT

instance MonadBase b m => MonadBase b (LoggingT t m) where
  liftBase = LoggingT . liftBase

instance (MonadBaseControl b m) => MonadBaseControl b (LoggingT t m) where
  type StM (LoggingT t m) a = ComposeSt (LoggingT t) m a
  liftBaseWith     = defaultLiftBaseWith
  restoreM         = defaultRestoreM

instance MonadResource m => MonadResource (LoggingT t m) where
  liftResourceT = LoggingT . liftResourceT

instance (MonadIO (HostFrame t), GameModule t m) => GameModule t (LoggingT t m) where
  type ModuleOptions t (LoggingT t m) = ModuleOptions t m
  runModule opts (LoggingT m) = do
    s <- emptyLoggingEnv
    runModule opts $ runReaderT m s
  withModule t _ = withModule t (Proxy :: Proxy m)

instance {-# OVERLAPPING #-} MonadAppHost t m => LoggingMonad t (LoggingT t m) where
  logMsgM lvl msg = do
    cntx <- ask
    fileOutput cntx lvl msg
    consoleOutput cntx lvl msg

  logMsgLnM lvl msg = do
    cntx <- ask
    let msg' = msg <> "\n"
    fileOutput cntx lvl msg'
    consoleOutput cntx lvl msg'

  logMsg lvl msgB = do
    cntx <- ask
    msg <- sample msgB
    fileOutput cntx lvl msg
    consoleOutput cntx lvl msg

  logMsgLn lvl msgB = do
    cntx <- ask
    msg <- sample msgB
    let msg' = msg <> "\n"
    fileOutput cntx lvl msg'
    consoleOutput cntx lvl msg'

  logMsgE lvl msgE = do
    cntx <- ask
    performEvent_ $ ffor msgE $ \msg -> do
      fileOutput cntx lvl msg
      consoleOutput cntx lvl msg

  logMsgLnE lvl msgE = do
    cntx <- ask
    performEvent_ $ ffor msgE $ \msg -> do
      let msg' = msg <> "\n"
      fileOutput cntx lvl msg'
      consoleOutput cntx lvl msg'

  loggingSetFile nm = do
    cntx <- ask
    let ref = loggingFileSink cntx
    sink <- readExternalRef ref
    whenJust sink $ liftIO . rmLoggerSet
    logger <- liftIO $ newFileLoggerSet defaultBufSize nm
    writeExternalRef ref (Just logger)

  loggingSetFilter l ss = do
    cntx <- ask
    modifyExternalRef (loggingFilter cntx) $ \lf -> let
      lfilter = case l `H.lookup` lf of
        Nothing -> H.insert l (HS.fromList ss) lf
        Just ss' -> H.insert l (HS.fromList ss `HS.union` ss') lf
      in (lfilter, ())

  -- | Enable/disable debugging mode
  loggingSetDebugFlag v = do
    cntx <- ask
    writeExternalRef (loggingDebug cntx) v

  -- | Return current value of debugging flag
  loggingDebugFlag = do
    cntx <- ask
    externalRefDynamic (loggingDebug cntx)

  {-# INLINE logMsgM #-}
  {-# INLINE logMsgLnM #-}
  {-# INLINE logMsg #-}
  {-# INLINE logMsgLn #-}
  {-# INLINE logMsgE #-}
  {-# INLINE logMsgLnE #-}
  {-# INLINE loggingSetFile #-}
  {-# INLINE loggingSetFilter #-}
  {-# INLINE loggingSetDebugFlag #-}
  {-# INLINE loggingDebugFlag #-}

-- | Output given message to logging file if allowed
fileOutput :: MonadIO m => LoggingEnv t -> LoggingLevel -> LogStr -> m ()
fileOutput ls ll msg = do
  sink <- readExternalRef (loggingFileSink ls)
  whenM (filterLogMessage ls ll LoggingFile) $
    whenJust sink $ \l -> liftIO $ pushLogStr l msg

-- | Output given message to console if allowed
consoleOutput :: MonadIO m => LoggingEnv t -> LoggingLevel -> LogStr -> m ()
consoleOutput ls ll msg = whenM (filterLogMessage ls ll LoggingConsole) $
  liftIO $ pushLogStr (loggingConsoleSink ls) msg

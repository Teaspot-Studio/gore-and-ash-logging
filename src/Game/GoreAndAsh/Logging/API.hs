{-|
Module      : Game.GoreAndAsh.Module
Description : Module that contains monadic and arrow API of logging module.
Copyright   : (c) Anton Gushcha, 2015-2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

Module that contains monadic and arrow API of logging module.
-}
module Game.GoreAndAsh.Logging.API(
    LoggingMonad(..)
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
  ) where

import Control.Monad.Extra (whenJust, whenM)
import Control.Monad.Reader
import Data.Monoid
import System.Log.FastLogger

import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS

import Game.GoreAndAsh
import Game.GoreAndAsh.Logging.State
import Game.GoreAndAsh.Logging.Module

-- | Shortcut for `toLogStr . show`
showl :: Show a => a -> LogStr
showl = toLogStr . show

-- | Low level API for module
class MonadAppHost t m => LoggingMonad t m | m -> t where
  -- | Put message to the console.
  logMsgM :: LoggingLevel -> LogStr -> m ()
  -- | Put message and new line to the console.
  logMsgLnM :: LoggingLevel -> LogStr -> m ()

  -- | Put message to the console.
  logMsg :: LoggingLevel -> Behavior t LogStr -> m ()
  -- | Put message and new line to the console.
  logMsgLn :: LoggingLevel -> Behavior t LogStr -> m ()

  -- | Put message to the log when the event fires
  logMsgE :: LoggingLevel -> Event t LogStr -> m ()
  -- | Put message and new line to log when the event fires
  logMsgLnE :: LoggingLevel -> Event t LogStr -> m ()

  -- | Setting current logging file handler
  loggingSetFile :: FilePath -> m ()

  -- | Setting allowed sinks for given logging level.
  --
  -- By default all messages are passed into file and console.
  loggingSetFilter :: LoggingLevel -> [LoggingSink] -> m ()

  -- | Enable/disable debugging mode
  loggingSetDebugFlag :: Bool -> m ()

  -- | Return current value of debugging flag
  loggingDebugFlag :: m (Dynamic t Bool)

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

instance {-# OVERLAPPABLE #-} (MonadAppHost t (mt m), LoggingMonad t m, MonadTrans mt) => LoggingMonad t (mt m) where
  logMsgM lvl msg = lift $ logMsgM lvl msg
  logMsgLnM lvl msg = lift $ logMsgLnM lvl msg
  logMsg lvl msgB = lift $ logMsg lvl msgB
  logMsgLn lvl msgB = lift $ logMsgLn lvl msgB
  logMsgE lvl msgB = lift $ logMsgE lvl msgB
  logMsgLnE lvl msgB = lift $ logMsgLnE lvl msgB
  loggingSetFile = lift . loggingSetFile
  loggingSetFilter a b = lift $ loggingSetFilter a b
  loggingSetDebugFlag = lift . loggingSetDebugFlag
  loggingDebugFlag = lift loggingDebugFlag

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

-- | Put message to console on every frame without newline
logDyn :: LoggingMonad t m => LoggingLevel -> Dynamic t LogStr -> m ()
logDyn lvl = logMsg lvl . current

-- | Put message to console on every frame
logDynLn :: LoggingMonad t m => LoggingLevel -> Dynamic t LogStr -> m ()
logDynLn lvl = logMsgLn lvl . current

-- | Put debug msg to console
logDebug :: LoggingMonad t m => Behavior t LogStr -> m ()
logDebug = logMsgLn LogDebug . fmap ("Debug: " <>)

-- | Put info msg to console
logInfo :: LoggingMonad t m => Behavior t LogStr -> m ()
logInfo = logMsgLn LogInfo . fmap ("Info: " <>)

-- | Put warn msg to console
logWarn :: LoggingMonad t m => Behavior t LogStr -> m ()
logWarn = logMsgLn LogWarn . fmap ("Warn: " <>)

-- | Put error msg to console
logError :: LoggingMonad t m => Behavior t LogStr -> m ()
logError = logMsgLn LogError . fmap ("Error: " <>)

-- | Put debug msg to console
logDebugDyn :: LoggingMonad t m => Dynamic t LogStr -> m ()
logDebugDyn = logDebug . current

-- | Put info msg to console
logInfoDyn :: LoggingMonad t m => Dynamic t LogStr -> m ()
logInfoDyn = logInfo . current

-- | Put warn msg to console
logWarnDyn :: LoggingMonad t m => Dynamic t LogStr -> m ()
logWarnDyn = logWarn . current

-- | Put error msg to console
logErrorDyn :: LoggingMonad t m => Dynamic t LogStr -> m ()
logErrorDyn = logError . current

-- | Put debug msg to console on event
logDebugE :: LoggingMonad t m => Event t LogStr -> m ()
logDebugE = logMsgLnE LogDebug . fmap ("Debug: " <>)

-- | Put info msg to console on event
logInfoE :: LoggingMonad t m => Event t LogStr -> m ()
logInfoE = logMsgLnE LogInfo . fmap ("Info: " <>)

-- | Put warn msg to console on event
logWarnE :: LoggingMonad t m => Event t LogStr -> m ()
logWarnE = logMsgLnE LogWarn . fmap ("Warn: " <>)

-- | Put error msg to console on event
logErrorE :: LoggingMonad t m => Event t LogStr -> m ()
logErrorE = logMsgLnE LogError . fmap ("Error: " <>)
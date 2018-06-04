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
{-# LANGUAGE LambdaCase #-}
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
  , logEither
  , logEitherWarn
  , logEitherError
  -- ** Verbose logging
  , logVerboseM
  , logVerbose
  , logVerboseDyn
  , logVerboseE
  ) where

import Control.Monad.Reader
import Data.Bifunctor
import Data.Monoid
import System.Log.FastLogger

import Game.GoreAndAsh
import Game.GoreAndAsh.Logging.State

-- | Shortcut for `toLogStr . show`
showl :: Show a => a -> LogStr
showl = toLogStr . show

-- | High-level API for module (intended to be used in components)
class MonadGame t m => LoggingMonad t m | m -> t where
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

instance {-# OVERLAPPABLE #-} (MonadGame t (mt m), LoggingMonad t m, MonadTrans mt) => LoggingMonad t (mt m) where
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

-- | Pass through events with possible failures and log them
logEither :: (LoggingMonad t m) => LoggingLevel -> Event t (Either LogStr a) -> m (Event t a)
logEither lvl e = do
  logMsgLnE lvl $ fforMaybe e $ \case
    Left msg -> Just msg
    _ -> Nothing
  return $ fforMaybe e $ \case
    Right a -> Just a
    _ -> Nothing

-- | Pass through events with possible failures and log them as warnings
logEitherWarn :: LoggingMonad t m => Event t (Either LogStr a) -> m (Event t a)
logEitherWarn = logEither LogWarn . fmap (first ("Warn: " <>))

-- | Pass through events with possible failures and log them as errors
logEitherError :: LoggingMonad t m => Event t (Either LogStr a) -> m (Event t a)
logEitherError = logEither LogError . fmap (first ("Error: " <>))

-- | Log only when detailed logging is switched on (once)
logVerboseM :: LoggingMonad t m => LogStr -> m ()
logVerboseM msg = do
  flag <- sample . current =<< loggingDebugFlag
  when flag $ logMsgLnM LogDebug ("Verbose: " <> msg)

-- | Log only when detailed logging is switched on (each frame)
logVerbose :: LoggingMonad t m => Behavior t LogStr -> m ()
logVerbose msg = do
  dynFlag <- loggingDebugFlag
  void $ networkView $ ffor dynFlag $ \flag -> if flag
    then logMsgLn LogDebug $ fmap ("Verbose: " <>) msg
    else return ()

-- | Log only when detailed logging is switched on (each frame)
logVerboseDyn :: LoggingMonad t m => Dynamic t LogStr -> m ()
logVerboseDyn = logVerbose . current

-- | Log only when detailed logging is switched on (each fire of event)
logVerboseE :: LoggingMonad t m => Event t LogStr -> m ()
logVerboseE msg = do
  dynFlag <- loggingDebugFlag
  void $ networkView $ ffor dynFlag $ \flag -> if flag
    then logMsgLnE LogDebug $ fmap ("Verbose: " <>) msg
    else return ()

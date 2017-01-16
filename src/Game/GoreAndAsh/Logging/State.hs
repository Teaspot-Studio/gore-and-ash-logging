{-|
Module      : Game.GoreAndAsh.Logging.State
Description : State of logging core module
Copyright   : (c) Anton Gushcha, 2015-2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

Contains description of state for logging core module.
-}
module Game.GoreAndAsh.Logging.State(
    LoggingEnv(..)
  , LoggingLevel(..)
  , LoggingSink(..)
  , emptyLoggingEnv
  , filterLogMessage
  ) where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics (Generic)
import System.Log.FastLogger
import Control.Monad.IO.Class

import Game.GoreAndAsh.Core

import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS

-- | Distanation of logging
data LoggingSink =
  -- | Putting to user terminal
    LoggingConsole
  -- | Putting into file
  | LoggingFile
  deriving (Eq, Ord, Bounded, Show, Read, Generic)

instance NFData LoggingSink
instance Hashable LoggingSink

-- | Describes important of logging message
data LoggingLevel =
  -- | Used for detailed logging
    LogDebug
  -- | Used for messages about normal operation of application
  | LogInfo
  -- | Used for recoverable errors or defaulting to fallback behavior
  | LogWarn
  -- | Used before throwing an exception or fatal fales
  | LogError
  -- | Special case of message, that never goes to console, but saved into file
  | LogMuted
  deriving (Eq, Ord, Bounded, Show, Read, Generic)

instance NFData LoggingLevel
instance Hashable LoggingLevel

type LoggingFilter = H.HashMap LoggingLevel (HS.HashSet LoggingSink)

-- | Inner state of logger.
--
-- [@t@] FRP engine implementation, can be ignored almost everywhere.
data LoggingEnv t = LoggingEnv {
  loggingFileSink    :: !(ExternalRef t (Maybe LoggerSet))
, loggingConsoleSink :: !LoggerSet
, loggingFilter      :: !(ExternalRef t LoggingFilter)
, loggingDebug       :: !(ExternalRef t Bool)
} deriving (Generic)

instance NFData (LoggingEnv t) where
  rnf LoggingEnv{..} =
     loggingFileSink `seq`
     loggingConsoleSink `seq`
     loggingFilter `seq`
     loggingDebug `seq` ()

-- | Create empty module state
emptyLoggingEnv :: MonadAppHost t m => m (LoggingEnv t)
emptyLoggingEnv = do
  fileSink <- newExternalRef Nothing
  lfilter <- newExternalRef mempty
  debugFlag <- newExternalRef False
  consoleSink <- liftIO $ newStdoutLoggerSet defaultBufSize
  return LoggingEnv {
      loggingFileSink = fileSink
    , loggingConsoleSink = consoleSink
    , loggingFilter = lfilter
    , loggingDebug = debugFlag
    }

-- | Returns 'True' if given message level is allowed to go in the sink
filterLogMessage :: MonadIO m => LoggingEnv s -> LoggingLevel -> LoggingSink -> m Bool
filterLogMessage LoggingEnv{..} ll ls = do
  lf <- readExternalRef loggingFilter
  return $ case H.lookup ll lf of
    Nothing -> True
    Just ss -> HS.member ls ss

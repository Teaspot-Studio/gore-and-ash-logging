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
    LoggingState(..)
  , LoggingLevel(..)
  , LoggingSink(..)
  , emptyLoggingState
  , filterLogMessage
  ) where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics (Generic)
import System.Log.FastLogger
import Control.Monad.IO.Class

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
-- [@s@] next state, states of modules are chained via nesting
data LoggingState = LoggingState {
  loggingFileSink    :: !(Maybe LoggerSet)
, loggingConsoleSink :: !LoggerSet
, loggingFilter      :: !LoggingFilter
, loggingDebug       :: !Bool
} deriving (Generic)

instance NFData LoggingState where
  rnf LoggingState{..} =
     loggingFileSink `seq`
     loggingConsoleSink `seq`
     loggingFilter `seq`
     loggingDebug `seq` ()

-- | Create empty module state
emptyLoggingState :: MonadIO m => m LoggingState
emptyLoggingState = do
  consoleSink <- liftIO $ newStdoutLoggerSet defaultBufSize
  return LoggingState {
      loggingFileSink = Nothing
    , loggingConsoleSink = consoleSink
    , loggingFilter = H.empty
    , loggingDebug = False
    }

-- | Returns 'True' if given message level is allowed to go in the sink
filterLogMessage :: LoggingState -> LoggingLevel -> LoggingSink -> Bool
filterLogMessage LoggingState{..} ll ls = case H.lookup ll loggingFilter of
  Nothing -> True
  Just ss -> HS.member ls ss

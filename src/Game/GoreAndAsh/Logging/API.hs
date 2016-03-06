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
  , loggingSetFile
  -- * Arrow API
  , logA
  , logALn
  , logE
  , logELn

  -- ** Every frame
  , logDebugA
  , logInfoA
  , logWarnA
  , logErrorA
  -- ** Event based
  , logDebugE
  , logInfoE
  , logWarnE
  , logErrorE

  -- ** Event tracing
  , traceEvent
  , traceEventShow
  ) where

import Control.Monad.Extra (whenJust)
import Control.Monad.State.Strict
import Control.Wire
import Data.Text
import Prelude hiding (id, (.))
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS
import qualified Data.Sequence as S
import System.IO as IO
import TextShow

import Game.GoreAndAsh
import Game.GoreAndAsh.Logging.State
import Game.GoreAndAsh.Logging.Module

-- | Low level API for module
class MonadIO m => LoggingMonad m where
  -- | Put message to the console.
  putMsgM :: LoggingLevel -> Text -> m ()
  -- | Put message and new line to the console.
  putMsgLnM :: LoggingLevel -> Text -> m ()
  -- | Setting current logging file handler
  loggingSetHandle :: IO.Handle -> m ()
  -- | Setting allowed sinks for given logging level.
  --
  -- By default all messages are passed into file and console.
  loggingSetFilter :: LoggingLevel -> [LoggingSink] -> m ()

instance {-# OVERLAPPING #-} MonadIO m => LoggingMonad (LoggingT s m) where
  putMsgM l t = do
    cntx <- get
    let newMsgs = case S.viewr $ loggingMsgs cntx of
          S.EmptyR -> loggingMsgs cntx S.|> (l, t)
          (s' S.:> (l', t')) -> s' S.|> (l', t' <> t)
    put $ cntx { loggingMsgs = newMsgs }

  putMsgLnM l t = do
    cntx <- get
    put $ cntx { loggingMsgs = loggingMsgs cntx S.|> (l, t) }

  loggingSetHandle h = do
    cntx <- get
    whenJust (loggingFile cntx) $ liftIO . IO.hClose
    put $ cntx { loggingFile = Just h }

  loggingSetFilter l ss = do
    cntx <- get
    let lfilter = case l `H.lookup` loggingFilter cntx of
          Nothing -> H.insert l (HS.fromList ss) . loggingFilter $ cntx
          Just ss' -> H.insert l (HS.fromList ss `HS.union` ss') . loggingFilter $ cntx
    put $ cntx { loggingFilter = lfilter }

instance {-# OVERLAPPABLE #-} (MonadIO (mt m), LoggingMonad m, MonadTrans mt) => LoggingMonad (mt m) where
  putMsgM a b = lift $ putMsgM a b
  putMsgLnM a b = lift $ putMsgLnM a b
  loggingSetHandle = lift . loggingSetHandle
  loggingSetFilter a b = lift $ loggingSetFilter a b

-- | Put message to console on every frame without newline
logA :: LoggingMonad m => LoggingLevel -> GameWire m Text ()
logA l = liftGameMonad1 (putMsgM l)

-- | Put message to console on every frame
logALn :: LoggingMonad m => LoggingLevel -> GameWire m Text ()
logALn l = liftGameMonad1 (putMsgLnM l)

-- | Put message to console on event without newline
logE :: LoggingMonad m => LoggingLevel -> GameWire m (Event Text) (Event ())
logE l = liftGameMonadEvent1 (putMsgM l)

-- | Put message to console on event
logELn :: LoggingMonad m => LoggingLevel -> GameWire m (Event Text) (Event ())
logELn l = liftGameMonadEvent1 (putMsgLnM l)

-- | Put info msg to console
logDebugA :: LoggingMonad m => GameWire m Text ()
logDebugA = logALn LogDebug . arr ("Debug: " <>)

-- | Put info msg to console
logInfoA :: LoggingMonad m => GameWire m Text ()
logInfoA = logALn LogInfo . arr ("Info: " <>)

-- | Put warn msg to console
logWarnA :: LoggingMonad m => GameWire m Text ()
logWarnA = logALn LogWarn . arr ("Warning: " <>)

-- | Put error msg to console
logErrorA :: LoggingMonad m => GameWire m Text ()
logErrorA = logALn LogError . arr ("Error: " <>)

-- | Put info msg to console on event
logDebugE :: LoggingMonad m => GameWire m (Event Text) (Event ())
logDebugE = logELn LogDebug . mapE ("Debug: " <>)

-- | Put info msg to console on event
logInfoE :: LoggingMonad m => GameWire m (Event Text) (Event ())
logInfoE = logELn LogInfo . mapE ("Info: " <>)

-- | Put warn msg to console on event
logWarnE :: LoggingMonad m => GameWire m (Event Text) (Event ())
logWarnE = logELn LogWarn . mapE ("Warning: " <>)

-- | Put error msg to console on event
logErrorE :: LoggingMonad m => GameWire m (Event Text) (Event ())
logErrorE = logELn LogError . mapE ("Error: " <>)

-- | Prints event with given function
traceEvent :: LoggingMonad m => (a -> Text) -> GameWire m (Event a) (Event ())
traceEvent f = logELn LogDebug . mapE f

-- | Prints event
traceEventShow :: (TextShow a, LoggingMonad m) => GameWire m (Event a) (Event ())
traceEventShow = traceEvent showt

-- | Helper to set logging file as local path
loggingSetFile :: (LoggingMonad m) => FilePath -- ^ Path to logging file
  -> Bool -- ^ If 'False', rewrites contents of the file, if 'True' opens in append mode
  -> m ()
loggingSetFile fname isAppend = do
  h <- liftIO $ IO.openFile fname $ if isAppend then AppendMode else WriteMode
  loggingSetHandle h

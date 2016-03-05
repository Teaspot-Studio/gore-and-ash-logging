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
import qualified Data.Sequence as S
import qualified Data.Text.IO as T
import qualified System.IO as IO

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
newtype LoggingT s m a = LoggingT { runLoggingT :: StateT (LoggingState s) m a }
  deriving (Functor, Applicative, Monad, MonadState (LoggingState s), MonadFix, MonadTrans, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadError e)

instance MonadBase IO m => MonadBase IO (LoggingT s m) where
  liftBase = LoggingT . liftBase

instance MonadResource m => MonadResource (LoggingT s m) where
  liftResourceT = LoggingT . liftResourceT

instance GameModule m s => GameModule (LoggingT s m) (LoggingState s) where
  type ModuleState (LoggingT s m) = LoggingState s
  runModule (LoggingT m) s = do
    ((a, s'), nextState) <- runModule (runStateT m s) (loggingNextState s)
    printAllMsgs s'
    return (a, s' {
        loggingMsgs = S.empty
      , loggingNextState = nextState
      })
    where
      printAllMsgs LoggingState{..} = liftIO $ mapM_ T.putStrLn loggingMsgs

  newModuleState = emptyLoggingState <$> newModuleState

  withModule _ = withModule (Proxy :: Proxy m)
  cleanupModule LoggingState{..} = case loggingFile of
    Nothing -> return ()
    Just h -> IO.hClose h

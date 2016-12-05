{-|
Module      : Game.GoreAndAsh.Console.Module
Description : Execution of console core module
Copyright   : (c) Anton Gushcha, 2015-2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

Contains description how to execute console core module.
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.GoreAndAsh.Console.Module(
    ConsoleT
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Proxy
import System.IO

import Game.GoreAndAsh
import Game.GoreAndAsh.Console.API

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
-- type AppMonad t a = ConsoleT t (GameMonad t)
--
-- app :: ConsoleMonad t m => m ()
-- app = do
--   e <- consolePrompt' "Enter a string: "
--   consolePutLn $ fmap ("You entered: " ++) e
-- @
--
-- See `examples/Example02.hs` for a full example.
newtype ConsoleT t m a = ConsoleT { runConsoleT :: m a }
  deriving (Functor, Applicative, Monad, MonadFix
    , MonadIO, MonadThrow, MonadCatch, MonadMask, MonadSample t, MonadHold t)

instance MonadTrans (ConsoleT t) where
  lift = ConsoleT

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (ConsoleT t m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger initializer = lift $ newFanEventWithTrigger initializer

instance MonadSubscribeEvent t m => MonadSubscribeEvent t (ConsoleT t m) where
  subscribeEvent = lift . subscribeEvent

instance MonadAppHost t m => MonadAppHost t (ConsoleT t m) where
  getFireAsync = lift getFireAsync
  getRunAppHost = do
    runner <- ConsoleT getRunAppHost
    return $ \m -> runner $ runConsoleT m
  performPostBuild_ = lift . performPostBuild_
  liftHostFrame = lift . liftHostFrame

instance MonadBase IO m => MonadBase IO (ConsoleT t m) where
  liftBase = ConsoleT . liftBase

instance MonadResource m => MonadResource (ConsoleT t m) where
  liftResourceT = ConsoleT . liftResourceT

instance GameModule t m => GameModule t (ConsoleT t m) where
  type ModuleOptions t (ConsoleT t m) = ModuleOptions t m
  runModule opts (ConsoleT m) = runModule opts m
  withModule t _ = withModule t (Proxy :: Proxy m)

instance {-# OVERLAPPING #-} MonadAppHost t m => ConsoleMonad t (ConsoleT t m) where
  consolePut emsg = performEvent $ ffor emsg $ \msg -> do
    liftIO $ putStr msg
    return msg

  consoleInput = do
    (e, fire) <- newExternalEvent
    _ <- liftIO . forkIO $ void $ fire =<< getLine
    return e

  consoleInputForever = do
    (e, fire) <- newExternalEvent
    tid <- liftIO . forkIO $ forever $ void $ fire =<< getLine
    return (e, killThread tid)

  consoleCharInput = do
    (e, fire) <- newExternalEvent
    _ <- liftIO . forkIO $ void $ fire =<< getChar
    return e

  consoleCharInputForever = do
    (e, fire) <- newExternalEvent
    tid <- liftIO . forkIO $ forever $ void $ fire =<< getChar
    return (e, killThread tid)

  consolePrompt msg = do
    (e, fire) <- newExternalEvent
    _ <- liftIO . forkIO $ do
      putStr msg
      hFlush stdout
      void $ fire =<< getLine
    return e

  consolePromptForever msg = do
    (e, fire) <- newExternalEvent
    tid <- liftIO . forkIO $ forever $ do
      putStr msg
      hFlush stdout
      void $ fire =<< getLine
    return (e, killThread tid)

  {-# INLINE consolePut #-}
  {-# INLINE consoleInput #-}
  {-# INLINE consoleInputForever #-}
  {-# INLINE consoleCharInput #-}
  {-# INLINE consoleCharInputForever #-}
  {-# INLINE consolePrompt #-}
  {-# INLINE consolePromptForever #-}

instance {-# OVERLAPPABLE #-} (MonadAppHost t (mt m), ConsoleMonad t m, MonadTrans mt) => ConsoleMonad t (mt m) where
  consolePut = lift . consolePut
  consoleInput = lift consoleInput
  consoleInputForever = lift consoleInputForever
  consoleCharInput = lift consoleCharInput
  consoleCharInputForever = lift consoleCharInputForever
  consolePrompt = lift . consolePrompt
  consolePromptForever = lift . consolePromptForever

  {-# INLINE consolePut #-}
  {-# INLINE consoleInput #-}
  {-# INLINE consoleInputForever #-}
  {-# INLINE consoleCharInput #-}
  {-# INLINE consoleCharInputForever #-}
  {-# INLINE consolePrompt #-}
  {-# INLINE consolePromptForever #-}
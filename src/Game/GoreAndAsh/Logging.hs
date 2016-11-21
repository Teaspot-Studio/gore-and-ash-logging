{-|
Module      : Game.GoreAndAsh.Logging
Description : Module that contains all logging API for Gore&Ash
Copyright   : (c) Anton Gushcha, 2015-2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

The module contains all API for Gore&Ash logging module. The module doesn't depends on
others core modules and could be place in any place in game monad stack.

The core module is not pure on it first phase and could be used with 'IO' as end monad.
See 'ModuleStack' documentation.

Example of embedding:

@
-- | Application monad is monad stack build from given list of modules over base monad (IO)
type AppStack = ModuleStack [LoggingT, ... other modules ... ] IO
newtype AppState = AppState (ModuleState AppStack)
  deriving (Generic)

instance NFData AppState

-- | Wrapper around type family
newtype AppMonad a = AppMonad (AppStack a)
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, LoggingMonad, ... other modules monads ... )

instance GameModule AppMonad AppState where
  type ModuleState AppMonad = AppState
  runModule (AppMonad m) (AppState s) = do
    (a, s') <- runModule m s
    return (a, AppState s')
  newModuleState = AppState <$> newModuleState
  withModule _ = withModule (Proxy :: Proxy AppStack)
  cleanupModule (AppState s) = cleanupModule s

-- | Arrow that is build over the monad stack
type AppWire a b = GameWire AppMonad a b
@
playerActor :: ActorMonad m => (PlayerId -> Player) -> GameActor m PlayerId Game Player
playerActor initialPlayer = makeActor $ \i -> stateWire (initialPlayer i) $ mainController i
  where
  mainController i = proc (g, p) -> do

@
-}
module Game.GoreAndAsh.Logging(
  -- * Low-level API
    LoggingState
  , LoggingT
  , LoggingLevel(..)
  , LoggingSink(..)
  , LoggingMonad(..)
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

import Game.GoreAndAsh.Logging.API
import Game.GoreAndAsh.Logging.Module
import Game.GoreAndAsh.Logging.State

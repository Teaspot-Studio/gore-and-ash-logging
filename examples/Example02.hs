module Main where

import Game.GoreAndAsh.Core
import Game.GoreAndAsh.Console
import Data.Proxy
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.Catch

type AppStack t = ConsoleT t (GameMonad t)

newtype AppMonad t a = AppMonad { runAppMonad :: AppStack t a}
  deriving (Functor, Applicative, Monad, MonadFix)

app :: ConsoleMonad t m => m ()
app = do
  e <- consolePromptForever' "Enter a string: "
  void $ consolePutLn $ fmap ("You entered: " ++) e

main :: IO ()
main = runSpiderHost $ hostApp $ runModule () (app :: AppMonad Spider ())

-- Boilerplate below

deriving instance (ReflexHost t, MonadCatch (HostFrame t)) => MonadCatch (AppMonad t)
deriving instance (ReflexHost t, MonadThrow (HostFrame t)) => MonadThrow (AppMonad t)
deriving instance (ReflexHost t, MonadMask (HostFrame t)) => MonadMask (AppMonad t)
deriving instance (ReflexHost t, MonadIO (HostFrame t)) => MonadIO (AppMonad t)
deriving instance (ReflexHost t, MonadIO (HostFrame t)) => ConsoleMonad t (AppMonad t)
deriving instance (ReflexHost t) => MonadSample t (AppMonad t)
deriving instance (ReflexHost t) => MonadHold t (AppMonad t)
deriving instance (ReflexHost t) => MonadSubscribeEvent t (AppMonad t)

instance ReflexHost t => MonadReflexCreateTrigger t (AppMonad t) where
  newEventWithTrigger = AppMonad . newEventWithTrigger
  newFanEventWithTrigger trigger = AppMonad $ newFanEventWithTrigger trigger

instance (ReflexHost t, MonadIO (HostFrame t)) => MonadAppHost t (AppMonad t) where
  getFireAsync = AppMonad getFireAsync
  getRunAppHost = do
    runner <- AppMonad getRunAppHost
    return $ \m -> runner $ runAppMonad m
  performPostBuild_ = AppMonad . performPostBuild_
  liftHostFrame = AppMonad . liftHostFrame

instance (ReflexHost t) => GameModule t (AppMonad t) where
  type ModuleOptions t (AppMonad t) = ()
  runModule _ m = runModule () $ runAppMonad m
  withModule t _ = withModule t (Proxy :: Proxy (AppStack t))

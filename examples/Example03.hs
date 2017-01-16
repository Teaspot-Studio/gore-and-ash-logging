{-# LANGUAGE RecursiveDo #-}
module Main where

import Control.Monad.Catch
import Control.Monad.Except
import Data.Proxy
import Game.GoreAndAsh.Console
import Game.GoreAndAsh.Core
import Text.Read

type AppStack t = ConsoleT t (GameMonad t)

newtype AppMonad t a = AppMonad { runAppMonad :: AppStack t a}
  deriving (Functor, Applicative, Monad, MonadFix)

app :: forall t m . ConsoleMonad t m => m ()
app = void askUser
  where
    -- loop = do
    --   rec nextDyn <- holdAppHost askUser $ const loop <$> nextE
    --       nextE <- sample $ current nextDyn
    --   return nextE

    askUser :: m (Event t ())
    askUser = do
      e <- consolePromptValidate "Enter a string: " validate
      shownE <- consolePutLn $ fmap (("You entered: " ++) . show) e
      return $ const () <$> shownE

    validate :: String -> Either String Int
    validate s = do
      (v :: Int) <- readEither s
      if v > 42
        then return v
        else Left "Value is less than 42!"

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

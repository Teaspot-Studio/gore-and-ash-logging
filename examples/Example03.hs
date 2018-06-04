{-# LANGUAGE RecursiveDo #-}
module Main where

import Control.Monad.Catch
import Control.Monad.Except
import Data.Proxy
import Game.GoreAndAsh.Console
import Game.GoreAndAsh.Core
import Text.Read

type AppMonad = GMSpider

app :: forall t m . MonadGame t m => m ()
app = void askUser
  where
    askUser :: m (Event t ())
    askUser = do
      e <- consolePromptValidate "Enter a string: " validate
      shownE <- consolePutLn $ fmap (("You entered: " ++) . show) e
      return $ const () <$> shownE

    validate :: String -> Either String Int
    validate s = do
      (v :: Int) <- readEither s
      if v >= 42
        then return v
        else Left "Value is less than 42!"

main :: IO ()
main = runGM (app :: AppMonad ())

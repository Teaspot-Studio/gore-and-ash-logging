{-|
Module      : Game.GoreAndAsh.Console
Description : Module that contains console interaction API for Gore&Ash
Copyright   : (c) Anton Gushcha, 2015-2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

The module contains an API for Gore&Ash console interaction module. The module
doesn't depends on others core modules and could be place in any place in game monad stack.

Example of embedding:

@
import Game.GoreAndAsh.Core
import Game.GoreAndAsh.Console

type AppMonad t a = ConsoleT t (GameMonad t) a

app :: MonadGame t m => m ()
app = do
  e <- consolePrompt' "Enter a string: "
  consolePutLn $ fmap ("You entered: " ++) e

main :: IO ()
main = runSpiderHost $ hostApp $ runModule () (app :: AppMonad Spider ())
@

For example with newtype see `examples/Example02.hs`.
-}
module Game.GoreAndAsh.Console(
    ConsoleEventStopper
  , consolePutLn
  , consoleInputForever'
  , consoleCharInputForever'
  , consolePromptForever'
  , consolePromptValidate
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Either
import Game.GoreAndAsh.Core
import System.IO

-- | Action that stops event producing
type ConsoleEventStopper = IO ()

-- | Print message to console
--
-- Returned event is fired when the message is printed.
consolePut :: MonadGame t m => Event t String -> m (Event t String)
consolePut emsg = performEvent $ ffor emsg $ \msg -> do
  liftIO $ putStr msg
  return msg

-- | Start expecting of console input.
consoleInput :: MonadGame t m => m (Event t String)
consoleInput = do
  (e, fire) <- newTriggerEvent
  _ <- liftIO . forkIO $ void $ fire =<< getLine
  return e

-- | Start expecting of console input. Repeats until is stopped.
--
-- An internal thread that watches user input is started. The second element
-- of result is action for stopping the internal event.
consoleInputForever :: MonadGame t m => m (Event t String, ConsoleEventStopper)
consoleInputForever = do
  (e, fire) <- newTriggerEvent
  tid <- liftIO . forkIO $ forever $ void $ fire =<< getLine
  return (e, killThread tid)

-- | Start expecting of console input. A single char input.
consoleCharInput :: MonadGame t m => m (Event t Char)
consoleCharInput = do
  (e, fire) <- newTriggerEvent
  _ <- liftIO . forkIO $ void $ fire =<< getChar
  return e

-- | Start expecting of console input. A single char input. Repeats until is stopped.
--
-- An internal thread that watches user input is started. The second element
-- of result is action for stopping the internal event.
consoleCharInputForever :: MonadGame t m => m (Event t Char, ConsoleEventStopper)
consoleCharInputForever = do
  (e, fire) <- newTriggerEvent
  tid <- liftIO . forkIO $ forever $ void $ fire =<< getChar
  return (e, killThread tid)

-- | Prints prompt and expects a user line input.
consolePrompt :: MonadGame t m => String -> m (Event t String)
consolePrompt msg = do
  (e, fire) <- newTriggerEvent
  _ <- liftIO . forkIO $ do
    putStr msg
    hFlush stdout
    void $ fire =<< getLine
  return e

-- | Prints prompt and expects a user line input. Repeats until is stopped.
--
-- Second argument signals when to ask again (for instance, instantly after printing of input string).
--
-- An internal thread that watches user input is started. The second element
-- of result is action for stopping the internal event.
consolePromptForever :: MonadGame t m => String -> Event t () -> m (Event t String, ConsoleEventStopper)
consolePromptForever msg nextE = do
  (e, fire) <- newTriggerEvent
  nextVar <- liftIO newEmptyMVar
  tid <- liftIO . forkIO $ forever $ do
    putStr msg
    hFlush stdout
    void $ fire =<< getLine
    takeMVar nextVar
  performEvent_ $ ffor nextE $ const . liftIO $ putMVar nextVar ()
  return (e, killThread tid)

-- | Start expecting of console input.
consoleInputForever' :: MonadGame t m => m (Event t String)
consoleInputForever' = fst <$> consoleInputForever

-- | Start expecting of console input. A single char input.
consoleCharInputForever' :: MonadGame t m => m (Event t Char)
consoleCharInputForever' = fst <$> consoleCharInputForever

-- | Same as 'consolePut' but with new line
--
-- Returned event is fired when the message is printed.
consolePutLn :: MonadGame t m => Event t String -> m (Event t String)
consolePutLn = consolePut . fmap (++ "\n")

-- | Prints prompt and expects a user line input, see 'consolePromptForever'
consolePromptForever' :: MonadGame t m => String -> Event t () -> m (Event t String)
consolePromptForever' msg nextE = fst <$> consolePromptForever msg nextE

-- | Prints prompt and fires event only when value passes through validation function.
consolePromptValidate :: forall t m a . MonadGame t m
  => String -- ^ Prompt message
  -> (String -> Either String a) -- ^ Validation function
  -> m (Event t a) -- ^ Fired once, when user successfully entered a value
consolePromptValidate msg validate = do
  res <- chain query
  return $ switchPromptlyDyn res
  where
  query :: m (Event t a, Chain t m (Event t a))
  query = do
    valE <- fmap validate <$> consolePrompt msg
    let succE = (\(Right a) -> a) <$> ffilter isRight valE
        failE = (\(Left e) -> "Invalid input: " ++ e) <$> ffilter isLeft valE
    failedE <- consolePutLn failE
    let nextE = fmap (const query) failedE
    return (succE, Chain nextE)

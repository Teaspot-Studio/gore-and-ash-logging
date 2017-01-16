{-|
Module      : Game.GoreAndAsh.Console.API
Description : API of console core module
Copyright   : (c) Anton Gushcha, 2015-2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

Contains description of API for console core module.
-}
{-# LANGUAGE RecursiveDo #-}
module Game.GoreAndAsh.Console.API(
    ConsoleEventStopper
  , ConsoleMonad(..)
  , consolePutLn
  , consoleInputForever'
  , consoleCharInputForever'
  , consolePromptForever'
  , consolePromptValidate
  ) where

import Data.Either
import Game.GoreAndAsh.Core

-- | Action that stops event producing
type ConsoleEventStopper = IO ()

class MonadAppHost t m => ConsoleMonad t m | m -> t where
  -- | Print message to console
  --
  -- Returned event is fired when the message is printed.
  consolePut :: Event t String -> m (Event t String)

  -- | Start expecting of console input.
  consoleInput :: m (Event t String)

  -- | Start expecting of console input. Repeats until is stopped.
  --
  -- An internal thread that watches user input is started. The second element
  -- of result is action for stopping the internal event.
  consoleInputForever :: m (Event t String, ConsoleEventStopper)

  -- | Start expecting of console input. A single char input.
  consoleCharInput :: m (Event t Char)

  -- | Start expecting of console input. A single char input. Repeats until is stopped.
  --
  -- An internal thread that watches user input is started. The second element
  -- of result is action for stopping the internal event.
  consoleCharInputForever :: m (Event t Char, ConsoleEventStopper)

  -- | Prints prompt and expects a user line input.
  consolePrompt :: String -> m (Event t String)

  -- | Prints prompt and expects a user line input. Repeats until is stopped.
  --
  -- An internal thread that watches user input is started. The second element
  -- of result is action for stopping the internal event.
  consolePromptForever :: String -> m (Event t String, ConsoleEventStopper)

-- | Start expecting of console input.
consoleInputForever' :: ConsoleMonad t m => m (Event t String)
consoleInputForever' = fst <$> consoleInputForever

-- | Start expecting of console input. A single char input.
consoleCharInputForever' :: ConsoleMonad t m => m (Event t Char)
consoleCharInputForever' = fst <$> consoleCharInputForever

-- | Same as 'consolePut' but with new line
--
-- Returned event is fired when the message is printed.
consolePutLn :: ConsoleMonad t m => Event t String -> m (Event t String)
consolePutLn = consolePut . fmap (++ "\n")

-- | Prints prompt and expects a user line input
consolePromptForever' :: ConsoleMonad t m => String -> m (Event t String)
consolePromptForever' msg = fst <$> consolePromptForever msg

-- | Prints prompt and fires event only when value passes through validation function.
consolePromptValidate :: forall t m a . ConsoleMonad t m
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

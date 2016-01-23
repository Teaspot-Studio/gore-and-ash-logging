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
  ) where

import qualified Data.Sequence as S
import Data.Text 
import GHC.Generics (Generic)
import Control.DeepSeq 

-- | Inner state of logger.
--
-- [@s@] next state, states of modules are chained via nesting
data LoggingState s = LoggingState {
  loggingMsgs :: !(S.Seq Text)
, loggingNextState :: !s
} deriving (Generic)

instance NFData s => NFData (LoggingState s)
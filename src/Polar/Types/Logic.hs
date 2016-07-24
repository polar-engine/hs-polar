{-# LANGUAGE Safe #-}

{-|
  Module      : Polar.Types.Logic
  Copyright   : (c) 2016 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : portable

  Abstraction layer for application logic.
-}

module Polar.Types.Logic where

import Control.Monad.RWS (RWS, runRWS)
import Polar.Types.Log (Priority)

type LogicEnv = ()

data LogicAction = LogicExitAction
                 | LogicLogWriteAction Priority String

type LogicOutput = [LogicAction]

data LogicState = LogicState
    { _logicStateTickFunctions :: [Logic ()]
    }

type Logic = RWS LogicEnv LogicOutput LogicState

defaultLogicState :: LogicState
defaultLogicState = LogicState
    { _logicStateTickFunctions = []
    }

runLogic :: Logic a -> LogicEnv -> LogicState -> (a, LogicState, LogicOutput)
runLogic = runRWS

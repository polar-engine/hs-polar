{-# LANGUAGE Safe #-}

{-|
  Module      : Polar.Types.Sys
  Copyright   : (c) 2015-2016 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : portable

  Abstraction layer for engine systems.
-}

module Polar.Types.Sys where

import Control.Monad.RWS (RWS, runRWS)
import {-# SOURCE #-} Polar.Types.Core (Core)
import Polar.Types.Logic
import Polar.Types.Log (Priority)

type SysEnv = ()

data SysAction = SysExitAction
               | SysLogWriteAction Priority String
               | SysCoreAction (Core ())

type SysOutput = [SysAction]

data SysState = SysState
    { _sysStateLogicState    :: LogicState
    , _sysStateTickFunctions :: [Sys ()]
    }

type Sys = RWS SysEnv SysOutput SysState

defaultSysState :: SysState
defaultSysState = SysState
    { _sysStateLogicState    = defaultLogicState
    , _sysStateTickFunctions = []
    }

runSys :: Sys a -> SysEnv -> SysState -> (a, SysState, SysOutput)
runSys = runRWS

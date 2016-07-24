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
import Polar.Types.Log (Priority)

data SysEnv = SysEnv
    { _sysEnvTickFunctions :: [Sys ()]
    }

data SysAction = SysExitAction
               | SysLogWriteAction Priority String
               | SysCoreAction (Core ())

type SysOutput = [SysAction]
data SysState = SysState {}
type Sys = RWS SysEnv SysOutput SysState

defaultSysEnv :: SysEnv
defaultSysEnv = SysEnv
    { _sysEnvTickFunctions = []
    }

defaultSysState :: SysState
defaultSysState = SysState {}

runSys :: Sys a -> SysEnv -> SysState -> (a, SysState, SysOutput)
runSys = runRWS

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
    , _sysStateSystems       :: [System]
    }

type Sys = RWS SysEnv SysOutput SysState

defaultSysState :: SysState
defaultSysState = SysState
    { _sysStateLogicState    = defaultLogicState
    , _sysStateSystems       = []
    }

runSys :: Sys a -> SysEnv -> SysState -> (a, SysState, SysOutput)
runSys = runRWS

data System = System
    { _systemName     :: String
    , _systemStartup  :: Sys ()
    , _systemTick     :: Sys ()
    , _systemShutdown :: Sys ()
    }

defaultSystem :: String -> System
defaultSystem name = System
    { _systemName     = name
    , _systemStartup  = pure ()
    , _systemTick     = pure ()
    , _systemShutdown = pure ()
    }

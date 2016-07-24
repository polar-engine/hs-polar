{-# LANGUAGE Safe #-}

{-|
  Module      : Polar.Types.Core
  Copyright   : (c) 2015-2016 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : portable

  Abstraction layer for engine core.
-}

module Polar.Types.Core where

import Control.Monad.RWS (RWST, runRWST)
import Polar.ConfigFile
import Polar.Types.Config
import Polar.Types.Sys

type CoreEnv = ()
type CoreOutput = ()

data CoreState = CoreState
    { _coreStateConfig           :: ConfigParser
    , _coreStateSysState         :: SysState
    , _coreStateSysTickFunctions :: [Sys ()]
    }

type Core = RWST CoreEnv CoreOutput CoreState IO

defaultCoreState :: CoreState
defaultCoreState = CoreState
    { _coreStateConfig           = defaultConfig
    , _coreStateSysState         = defaultSysState
    , _coreStateSysTickFunctions = []
    }

runCore :: Core a -> CoreEnv -> CoreState -> IO (a, CoreState, CoreOutput)
runCore = runRWST

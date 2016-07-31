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
import Polar.Types.Storage

type CoreEnv = ()
type CoreOutput = ()

data CoreState = CoreState
    { _coreStateConfig     :: ConfigParser
    , _coreStateSysState   :: SysState
    , _coreStateStorage    :: Storage
    , _coreStateShouldExit :: Bool
    }

type Core = RWST CoreEnv CoreOutput CoreState IO

defaultCoreState :: CoreState
defaultCoreState = CoreState
    { _coreStateConfig     = defaultConfig
    , _coreStateSysState   = defaultSysState
    , _coreStateStorage    = defaultStorage
    , _coreStateShouldExit = False
    }

runCore :: Core a -> CoreEnv -> CoreState -> IO (a, CoreState, CoreOutput)
runCore = runRWST

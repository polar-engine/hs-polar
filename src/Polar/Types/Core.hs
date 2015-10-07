{-# LANGUAGE Safe #-}

{-|
  Module      : Polar.Types.Core
  Copyright   : (c) 2015 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : portable

  Engine core abstraction types.
-}

module Polar.Types.Core where

import Control.Monad.RWS (RWST, runRWST)
import Polar.ConfigFile
import Polar.Types.Config
import Polar.Types.LL

type CoreEnv = ()
type CoreOutput = ()

data CoreState = CoreState
    { _coreStateConfig          :: ConfigParser
    , _coreStateLlState         :: LLState
    , _coreStateLlTickFunctions :: [PolarLL [PolarCore ()]]
    }

type PolarCore = RWST CoreEnv CoreOutput CoreState IO

defaultCoreState :: CoreState
defaultCoreState = CoreState
    { _coreStateConfig          = defaultConfig
    , _coreStateLlState         = defaultLLState
    , _coreStateLlTickFunctions = []
    }

runCore :: PolarCore a -> CoreEnv -> CoreState -> IO (a, CoreState, CoreOutput)
runCore = runRWST

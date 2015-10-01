{-# LANGUAGE Safe #-}

{-|
  Module      : Polar.Types.LL
  Copyright   : (c) 2015 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : portable

  Low-level engine abstraction types.
-}

module Polar.Types.LL where

import Control.Monad.RWS (RWS, runRWS)
import {-# SOURCE #-} Polar.Types.Core (PolarCore)

data LLEnv = LLEnv
    { _lLEnvTickFunctions :: [PolarLL [PolarCore ()]]
    }

type LLOutput = ()

data LLState = LLState {}

type PolarLL = RWS LLEnv LLOutput LLState

defaultLLEnv :: LLEnv
defaultLLEnv = LLEnv
    { _lLEnvTickFunctions = []
    }

defaultLLState :: LLState
defaultLLState = LLState {}

runLL :: PolarLL a -> LLEnv -> LLState -> (a, LLState, LLOutput)
runLL = runRWS

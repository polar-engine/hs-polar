{-# LANGUAGE Safe #-}

{-|
  Module      : Polar.Core.Run
  Copyright   : (c) 2015 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : portable

  This module defines functions for running the engine core.
-}

module Polar.Core.Run (run, loop) where

import Control.Monad.RWS (liftIO)
import Control.Concurrent (threadDelay)
import Control.Lens (use)
import Polar.Types
import Polar.LL.Run (tick)

run :: PolarCore ()
run = loop

loop :: PolarCore ()
loop = do
    (core, _, _) <- runLL tick . LLEnv <$> use llTickFunctions <*> use llState
    sequence_ core
    liftIO (threadDelay 10)
    loop

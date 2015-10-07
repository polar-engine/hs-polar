{-# LANGUAGE Safe #-}
{-# LANGUAGE LambdaCase #-}

{-|
  Module      : Polar.Core.Run
  Copyright   : (c) 2015 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : non-portable (GHC extensions)

  This module defines functions for running the engine core.
-}

module Polar.Core.Run (run, loop) where

import Control.Monad.RWS (liftIO)
import Control.Concurrent (threadDelay)
import Control.Lens.Getter (use)
import Polar.Types
import Polar.Core.Config
import Polar.Core.Log
import Polar.LL.Run (tick)

run :: PolarCore ()
run = setup *> loop

setup :: PolarCore ()
setup = do
    logCore INFO "setting up core systems"
    setupConfig
    setupLog

loop :: PolarCore ()
loop = do
    (core, _, _) <- runLL tick . LLEnv <$> use llTickFunctions <*> use llState
    sequence_ core
    getConfig integerOption "Core" "TimeToSleep" >>= liftIO . threadDelay . fromInteger
    loop

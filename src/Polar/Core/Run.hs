{-# LANGUAGE Safe #-}
{-# LANGUAGE LambdaCase #-}

{-|
  Module      : Polar.Core.Run
  Copyright   : (c) 2015-2016 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : non-portable (GHC extensions)

  This module defines functions for running the engine core.
-}

module Polar.Core.Run (run, loop) where

import Data.Foldable (traverse_)
import Control.Monad.RWS (liftIO)
import Control.Concurrent (threadDelay)
import Control.Lens.Getter (use)
import Polar.Types
import Polar.Core.Config
import Polar.Log (setupLog, logWrite)
import Polar.Sys.Run (tickSys)

run :: Core ()
run = setup *> loop

setup :: Core ()
setup = do
    logWrite INFO "setting up core systems"
    setupConfig
    setupLog

loop :: Core ()
loop = do
    (_, _, sysActs) <- runSys tickSys () <$> use sysState
    traverse_ runSysAction sysActs
    liftIO . threadDelay . fromInteger =<< getConfig integerOption "Core" "TimeToSleep"
    loop

runSysAction :: SysAction -> Core ()
runSysAction SysExitAction = undefined
runSysAction (SysLogWriteAction priority msg) = logWrite priority msg
runSysAction (SysCoreAction core) = core

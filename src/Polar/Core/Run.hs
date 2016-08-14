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
import Data.Dynamic
import Control.Monad.RWS (void, unless, liftIO)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan
import Polar.Types
import Polar.Core.Config
import Polar.Storage
import Polar.Log (startupLog, logWrite)
import Polar.Exit (exit)
import Polar.Sys.Run

run :: Core ()
run = startupCore *> loop *> shutdownCore

startupCore :: Core ()
startupCore = do
    logWrite DEBUG "Starting up Config"
    startupConfig
    logWrite DEBUG "Starting up Log"
    startupLog
    (_, _, sysActs) <- runSys startupSys () <$> use sysState
    traverse_ runSysAction sysActs

loop :: Core ()
loop = do
    handleMsgs
    (_, _, sysActs) <- runSys tickSys () <$> use sysState
    traverse_ runSysAction sysActs
    liftIO . threadDelay . fromInteger =<< getConfig integerOption "Core" "TimeToSleep"
    flip unless loop =<< use shouldExit

handleMsgs :: Core ()
handleMsgs = use msgQueue >>= liftIO . atomically . tryReadTChan >>= \case
    Nothing -> pure ()
    Just msg -> handleMsg msg *> handleMsgs

shutdownCore :: Core ()
shutdownCore = do
    (_, _, sysActs) <- runSys shutdownSys () <$> use sysState
    traverse_ runSysAction sysActs
    logWrite DEBUG "Shutting down Log"
    logWrite DEBUG "Shutting down Config"

handleMsg :: CoreMsg -> Core ()
handleMsg (CoreStoreMsg Nothing x)         = void (store x)
handleMsg (CoreStoreMsg (Just (rep, h)) x) = storeKey rep h (dynTypeRep x) =<< storeDyn x

runSysAction :: SysAction -> Core ()
runSysAction SysExitAction = exit
runSysAction (SysLogWriteAction priority msg) = logWrite priority msg
runSysAction (SysCoreAction core) = core

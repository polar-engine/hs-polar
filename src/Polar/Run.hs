{-# LANGUAGE Safe #-}

{-|
  Module      : Polar.Run
  Copyright   : (c) 2015-2016 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : portable

  Simplified helper functions to run the engine.
-}

module Polar.Run (run) where

import Control.Monad (void)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan
import Polar.Types
import qualified Polar.Core.Run as C (run)

-- |Run the engine using the given initial state.
run :: Engine -> IO ()
run engine = do
    chan <- atomically newTChan
    void $ runCore C.run () $ defaultCoreState chan
        & sysState.logicState.tickFunctions .~ engine^.logicTicks
        & sysState.systems                  .~ engine^.systems

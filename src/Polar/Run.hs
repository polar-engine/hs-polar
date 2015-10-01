{-# LANGUAGE Safe #-}

{-|
  Module      : Polar.Run
  Copyright   : (c) 2015 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : portable

  Simplified helper functions to run the engine.
-}

module Polar.Run (run) where

import Control.Monad (void)
import Polar.Types.Core (defaultCoreState, runCore)
import qualified Polar.Core.Run as C (run)

-- |Run the engine using the default initial state.
run :: IO ()
run = void $ runCore C.run () defaultCoreState

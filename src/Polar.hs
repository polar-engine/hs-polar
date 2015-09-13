{-# LANGUAGE Safe #-}

{-|
  Module      : Polar
  Copyright   : (c) 2015 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : portable

  Simplified helper functions to run the engine.
-}

module Polar (runDefault, run) where

import Control.Monad.State (evalStateT)
import Polar.Types (Engine, defaultEngine)
import qualified Polar.Engine as E (run)

-- |Run the engine using the default initial state.
runDefault :: IO ()
runDefault = run defaultEngine

-- |Run the engine using a specified initial state.
run :: Engine -- ^ initial state
    -> IO ()
run = evalStateT E.run

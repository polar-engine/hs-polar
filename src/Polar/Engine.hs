{-# LANGUAGE Safe #-}

{-|
  Module      : Polar.Engine
  Copyright   : (c) 2015 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : portable

  This module exposes functions that comprise the main loop of the engine.
-}

module Polar.Engine (run, loop) where

import Data.Function.Apply
import Control.Monad.Truthful (unlessTruthful)
import Control.Lens ((.=), use)
import Polar.Types
import Polar.Listener

-- |Run the engine within the 'Polar' monad.
run :: PolarT IO ()
run = do
    hoistState $ do
        use startup >>= mapM_ (listen "startup")
        notify "startup" ()
    loop
    hoistState $ notify "shutdown" ()

-- |Run the engine loop until an exit is requested.
loop :: PolarT IO ()
loop = use willExit >>= unlessTruthful `apply` do
    hoistState $ notify "tick" ()
    actions <- use deferredIO
    deferredIO .= []
    sequence actions
    loop

{-# LANGUAGE Safe #-}

{-|
  Module      : Polar.Engine
  Copyright   : (c) 2015 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : portable

  This module exposes functions that comprise the main loop of the engine.
-}

module Polar.Engine where

import Control.Monad.Truthful (unlessTruthful)
import Control.Lens (use)
import Polar.Types
import Polar.Listener

-- |Run the engine within the 'PolarIO' monad.
run :: PolarIO ()
run = do
    use startup >>= mapM_ (listen "startup")
    notify "startup" ()
    loop
    notify "shutdown" ()

-- |Run the engine loop until an exit is requested.
loop :: PolarIO ()
loop = use willExit >>= unlessTruthful (notify "tick" () >> loop)

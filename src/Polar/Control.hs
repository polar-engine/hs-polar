{-# LANGUAGE Safe #-}

{-|
  Module      : Polar.Control
  Copyright   : (c) 2015 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : portable

  Helper functions to control the functioning of the engine.
-}

module Polar.Control (exit) where

import Control.Lens ((.=))
import Polar.Types

-- |Exit out of the engine after the current tick.
exit :: PolarIO ()
exit = willExit .= True

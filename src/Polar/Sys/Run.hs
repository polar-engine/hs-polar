{-# LANGUAGE Safe #-}

{-|
  Module      : Polar.Sys.Run
  Copyright   : (c) 2015-2016 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : portable

  This module defines functions for running the engine systems abstraction layer.
-}

module Polar.Sys.Run (tick) where

import Control.Lens.Getter (view)
import Polar.Types

tick :: Sys ()
tick = view tickFunctions >>= sequence_

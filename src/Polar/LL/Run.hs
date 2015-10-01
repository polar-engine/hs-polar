{-# LANGUAGE Safe #-}

{-|
  Module      : Polar.LL.Run
  Copyright   : (c) 2015 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : portable

  This module defines functions for running the low-level engine.
-}

module Polar.LL.Run (tick) where

import Control.Lens (view)
import Polar.Types

tick :: PolarLL [PolarCore ()]
tick = concat <$> (view tickFunctions >>= sequence)

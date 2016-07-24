{-# LANGUAGE Safe #-}

{-|
  Module      : Polar.Logic.Run
  Copyright   : (c) 2016 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : portable

  This module defines functions for running the application logic abstraction layer.
-}

module Polar.Logic.Run (tickLogic) where

import Control.Lens.Getter (use)
import Polar.Types

tickLogic :: Logic ()
tickLogic = sequence_ =<< use tickFunctions

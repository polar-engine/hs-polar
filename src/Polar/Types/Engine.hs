{-# LANGUAGE Safe #-}

{-|
  Module      : Polar.Types.Engine
  Copyright   : (c) 2016 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : portable

  Engine input parameters.
-}

module Polar.Types.Engine where

import Polar.Types.Sys
import Polar.Types.Logic

data Engine = Engine
    { _engineSysTicks   :: [Sys ()]
    , _engineLogicTicks :: [Logic ()]
    }

defaultEngine :: Engine
defaultEngine = Engine
    { _engineSysTicks   = []
    , _engineLogicTicks = []
    }

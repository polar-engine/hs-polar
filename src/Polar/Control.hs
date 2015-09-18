{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
  Module      : Polar.Control
  Copyright   : (c) 2015 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : portable

  Helper functions to control the functioning of the engine.
-}

module Polar.Control (io, exit) where

import Control.Lens ((.=), (%=))
import Polar.Types

-- |Schedule IO to be performed after the current tick.
io :: MonadPolar m => PolarT IO () -> m ()
io action = deferredIO %= (action :)

-- |Exit out of the engine after the current tick.
exit :: MonadPolar m => m ()
exit = willExit .= True

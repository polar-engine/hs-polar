{-# LANGUAGE Trustworthy #-}

{-|
  Module      : Polar.Run
  Copyright   : (c) 2015-2016 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : portable

  Simplified helper functions to run the engine.
-}

module Polar.Run (run) where

import Control.Monad (void)
import Control.Lens ((&), (.~))
import Polar.Types
import Polar.Log
import qualified Polar.Core.Run as C (run)

-- |Run the engine using the default initial state.
run :: IO ()
run = void $ runCore C.run () $
    defaultCoreState&sysState.logicState.tickFunctions .~ [logWrite DEBUG "hello"]

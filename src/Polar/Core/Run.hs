{-# LANGUAGE Safe #-}
{-# LANGUAGE LambdaCase #-}

{-|
  Module      : Polar.Core.Run
  Copyright   : (c) 2015 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : non-portable (GHC extensions)

  This module defines functions for running the engine core.
-}

module Polar.Core.Run (run, loop) where

import Control.Monad.RWS (liftIO)
import Control.Concurrent (threadDelay)
import Control.Lens.Getter (use)
import Polar.Types
import Polar.Core.Config
import Polar.LL.Run (tick)
import System.IO

run :: PolarCore ()
run = loop

loop :: PolarCore ()
loop = do
    (core, _, _) <- runLL tick . LLEnv <$> use llTickFunctions <*> use llState
    sequence_ core
    getConfig integerOption "Core" "TimeToSleep" >>= \case
        Left (_, err) -> liftIO (putStrLn err) -- TODO: do something better here
        Right i       -> liftIO (threadDelay (fromInteger i))
    loop

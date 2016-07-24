{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
  Module      : Polar.Exit
  Copyright   : (c) 2016 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : non-portable (GHC extensions)

  Exit functions.
-}

module Polar.Exit where

import Control.Monad.RWS (tell)
import Polar.Types
import Polar.Log (logWrite)

class Monad m => ExitPolar m where exit :: m ()

instance ExitPolar Core  where exit = logWrite INFO "exiting" *> undefined
instance ExitPolar Sys   where exit = tell [SysExitAction]
instance ExitPolar Logic where exit = tell [LogicExitAction]

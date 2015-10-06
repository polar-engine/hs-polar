{-# LANGUAGE Safe #-}

{-|
  Module      : Polar.Types.Log
  Copyright   : (c) 2015 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : portable

  Logging functions.
-}

module Polar.Types.Log where

import Polar.Types.Config (ConfigProxy(..))

data Priority = TRACE | DEBUG | VERBOSE | INFO | NOTICE | WARNING | ERROR | CRITICAL | FATAL
                deriving (Eq, Ord, Read, Show)

priorityOption :: ConfigProxy Priority
priorityOption = ConfigProxy

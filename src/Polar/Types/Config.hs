{-# LANGUAGE Safe #-}

{-|
  Module      : Polar.Types.Config
  Copyright   : (c) 2015 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : portable

  Engine configuration types.
-}

module Polar.Types.Config where

import Polar.ConfigFile

defaultConfig = "Core"     |$| "TimeToSleep"  |=| "10"
            |%| "Renderer" |$| "FieldOfView"  |=| "70"
                           |$| "ViewDistance" |=| "1000"
                           |$| "SSAO"         |=| "yes"
            |%| "Mixer"    |$| "Muted"        |=| "no"

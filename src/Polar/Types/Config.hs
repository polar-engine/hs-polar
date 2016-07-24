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

defaultConfig = emptyCP { optionNameTransform = id }
            |%| "Core"     |$| "TimeToSleep"  |=| "1000"
            |%| "Log"      |$| "Level"        |=| "DEBUG"
            |%| "Renderer" |$| "FieldOfView"  |=| "70"
                           |$| "ViewDistance" |=| "1000"
                           |$| "SSAO"         |=| "yes"
            |%| "Mixer"    |$| "Muted"        |=| "no"

data ConfigProxy a = ConfigProxy

stringOption :: ConfigProxy String
stringOption = ConfigProxy

boolOption :: ConfigProxy Bool
boolOption = ConfigProxy

integerOption :: ConfigProxy Integer
integerOption = ConfigProxy

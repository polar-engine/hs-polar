{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Polar.Types.Lenses where

import Control.Lens.TH (makeFields)
import Polar.Types.Point
import Polar.Types.Box
import Polar.Types.Core
import Polar.Types.Sys
import Polar.Types.Logic

makeFields ''Point
makeFields ''Box
makeFields ''CoreState
makeFields ''SysState
makeFields ''LogicState

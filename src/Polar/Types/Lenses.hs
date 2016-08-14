{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Polar.Types.Lenses where

import Control.Lens (nearly)
import Control.Lens.Empty
import Control.Lens.TH (makeFields)
import Polar.Types.Point
import Polar.Types.Box
import Polar.Types.Core
import Polar.Types.Sys
import Polar.Types.Logic
import Polar.Types.Engine
import Polar.Types.Storage

makeFields ''Point
makeFields ''Box
makeFields ''CoreState
makeFields ''SysState
makeFields ''System
makeFields ''LogicState
makeFields ''Engine
makeFields ''VectorStorage

instance AsEmpty VectorStorage where
    _Empty = nearly defaultVectorStorage vectorStorageNull

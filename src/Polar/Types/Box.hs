{-# LANGUAGE Safe #-}

{-|
  Module      : Polar.Types.Box
  Copyright   : (c) 2015 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : portable

  This module defines the box type.
-}

module Polar.Types.Box where

import Polar.Types.Point

-- |Representation of a box with an origin and a size.
data Box a = Box { _boxOrigin :: Point a
                 , _boxSize   :: Point a
                 } deriving (Eq, Show)

-- |Default value for 'Box'.
defaultBox :: Num a => Box a
defaultBox = Box defaultPoint defaultPoint

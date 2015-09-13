{-# LANGUAGE Safe #-}

module Polar.Types.Box where

import Polar.Types.Point

data Box a = Box { _boxOrigin :: Point a
                 , _boxSize   :: Point a
                 } deriving (Eq, Show)

defaultBox :: Num a => Box a
defaultBox = Box defaultPoint defaultPoint

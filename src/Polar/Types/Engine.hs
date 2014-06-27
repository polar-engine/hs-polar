module Polar.Types.Engine where

import Polar.Types.Point2
import Polar.Types.Rectangle

newtype Engine = Engine
    { viewport  :: Rectangle Int
    } deriving (Show)

defaultEngine :: Engine
defaultEngine = Engine
    { viewport = Rectangle (Point2 0 0) (Point2 1280 720)
    }

mapViewport :: (Rectangle Int -> Rectangle Int) -> Engine -> Engine
mapViewport f v = v {viewport = f (viewport v)}

module Polar.Types.Engine where

import Polar.Types.Point2
import Polar.Types.Rectangle

data Engine = Engine
    { title     :: String
    , viewport  :: Rectangle Int
    } deriving (Show)

defaultEngine :: Engine
defaultEngine = Engine
    { title     = "Polar Engine 4"
    , viewport  = Rectangle (Point2 0 0) (Point2 1280 720)
    }

mapViewport :: (Rectangle Int -> Rectangle Int) -> Engine -> Engine
mapViewport f v = v {viewport = f (viewport v)}

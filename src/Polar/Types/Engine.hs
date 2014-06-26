module Polar.Types.Engine where

data Engine = Engine
    { width     :: Int
    , height    :: Int
    } deriving (Show)

defaultEngine :: Engine
defaultEngine = Engine
    { width     = 1280
    , height    = 720
    }

mapWidth :: (Int -> Int) -> Engine -> Engine
mapWidth f e = e {width = f (width e)}

mapHeight :: (Int -> Int) -> Engine -> Engine
mapHeight f e = e {height = f (height e)}

module Polar.Engine where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
import Polar.Types.Engine
import Polar.Types.Rectangle
import Polar.Types.Point2

run :: StateT Engine IO ()
run = do
    modify $ (mapViewport . mapSize . mapX) (`quot` 2)
    gets (x . size . viewport) >>= liftIO . putStrLn . ("width = " ++) . show
    get >>= liftIO . print

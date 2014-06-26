module Polar.Engine where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
import Polar.Types.Engine

run :: StateT Engine IO ()
run = do
    modify $ mapWidth (`quot` 2) . mapHeight (`quot` 2)
    gets width >>= liftIO . putStrLn . ("width = " ++) . show
    get >>= liftIO . print

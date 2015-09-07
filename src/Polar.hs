module Polar (run, runDefault) where

import Control.Monad.State (evalStateT)
import Polar.Types (Engine, defaultEngine)
import qualified Polar.Engine as E (run)

run :: Engine -> IO ()
run = evalStateT E.run

runDefault :: IO ()
runDefault = run defaultEngine

{-# LANGUAGE LambdaCase #-}

module Polar.Engine where

import Data.Function.Apply
import Control.Monad (unless)
import Control.Lens (use)
import Polar.Types
import Polar.Listener

run :: PolarIO ()
run = do
    use startup >>= mapM_ (listen "startup")
    notify "startup" ()
    loop
    notify "shutdown" ()

loop :: PolarIO ()
loop = use willExit >>= flip unless `apply` do
    notify "tick" ()
    loop

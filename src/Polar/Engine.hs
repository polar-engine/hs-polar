{-# LANGUAGE LambdaCase #-}

module Polar.Engine where

import Data.Function.Apply
import Control.Monad (unless)
import Control.Lens (use)
import Polar.Types
import Polar.Listener

run :: PolarIO ()
run = do
    use startup >>= mapM_ (listen StartupNote)
    notify StartupNote ()
    loop
    notify ShutdownNote ()

loop :: PolarIO ()
loop = use willExit >>= flip unless `apply` do
    notify TickNote ()
    loop

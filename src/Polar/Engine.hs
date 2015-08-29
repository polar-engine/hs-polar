{-# LANGUAGE LambdaCase #-}

module Polar.Engine where

import Data.Function.Apply
import Control.Monad.State
import Control.Lens (use)
import System.IO (hSetBuffering, stdout, stderr, BufferMode(..))
import Polar.Types
import Polar.Listener

run :: PolarIO ()
run = do
    liftIO $ do
        hSetBuffering stdout NoBuffering
        hSetBuffering stderr LineBuffering
    use startup >>= mapM_ (listen StartupNote)
    notify StartupNote ()
    loop
    notify ShutdownNote ()

loop :: PolarIO ()
loop = use willExit >>= flip unless `apply` do
    notify TickNote ()
    loop

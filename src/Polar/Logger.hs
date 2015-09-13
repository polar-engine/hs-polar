{-# LANGUAGE Safe #-}

module Polar.Logger where

import Control.Monad.State (liftIO)
import System.IO (BufferMode(..), stdout, stderr, hPutStrLn, hSetBuffering)
import GHC.Stack (currentCallStack, renderStack)
import Polar.Types
import Polar.Listener
import Polar.Control

startup :: ListenerF ()
startup _ _ = do
    liftIO (hSetBuffering stdout NoBuffering)
    liftIO (hSetBuffering stderr LineBuffering)
    listen "error" (Listener onError)

onError :: ListenerF String
onError _ err = do
    stk <- liftIO currentCallStack
    liftIO $ hPutStrLn stderr ("[ERROR] " ++ err ++ '\n' : renderStack stk)
    exit
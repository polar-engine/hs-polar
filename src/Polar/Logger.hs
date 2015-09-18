{-# LANGUAGE Safe #-}

{-|
  Module      : Polar.Logger
  Copyright   : (c) 2015 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : non-portable (GHC.Stack)

  This module exposes a startup event listener which adds an error listener to log all errors to 'stderr'.
-}

module Polar.Logger (startup) where

import Control.Monad.State (liftIO)
import System.IO (BufferMode(..), stdout, stderr, hPutStrLn, hSetBuffering)
import GHC.Stack (currentCallStack, renderStack)
import Polar.Types hiding (startup)
import Polar.Listener
import Polar.Control

-- |Startup event listener.
startup :: Listener ()
startup _ _ = do
    liftIO (hSetBuffering stdout NoBuffering)
    liftIO (hSetBuffering stderr LineBuffering)
    listen "error" (ExListener onError)

-- |Error event listener.
onError :: Listener String
onError _ err = do
    stk <- liftIO currentCallStack
    liftIO $ hPutStrLn stderr ("[ERROR] " ++ err ++ '\n' : renderStack stk)
    exit

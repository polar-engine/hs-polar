module Polar.Logger where

import Control.Monad.State (liftIO)
import System.IO (BufferMode(..), stdout, stderr, hPutStrLn, hSetBuffering)
import GHC.Stack (currentCallStack, renderStack)
import Polar.Types
import Polar.Listener

startup :: ListenerF ()
startup _ _ = do
    liftIO (hSetBuffering stdout NoBuffering)
    liftIO (hSetBuffering stderr LineBuffering)
    listen ErrorNote (Listener onError)

onError :: ListenerF String
onError ErrorNote err = do
    stk <- liftIO currentCallStack
    liftIO $ hPutStrLn stderr ("[ERROR] " ++ err ++ '\n' : renderStack stk)
onError _ _ = return ()

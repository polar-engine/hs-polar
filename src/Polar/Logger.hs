module Polar.Logger where

import Control.Monad.State (liftIO)
import System.IO (stderr, hPutStrLn)
import GHC.Stack (currentCallStack, renderStack)
import Polar.Types
import Polar.Listener

startup :: ListenerF ()
startup _ _ = listen ErrorNote (Listener onError)

onError :: ListenerF String
onError ErrorNote err = do
    stk <- renderStack <$> liftIO currentCallStack
    liftIO $ hPutStrLn stderr ("[ERROR] " ++ err ++ '\n' : stk)
onError _ _ = return ()
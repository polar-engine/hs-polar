module Polar.Logger where

import Control.Monad.State (liftIO)
import System.IO (stderr, hPutStrLn)
import Polar.Types
import Polar.Listener

startup :: Listener
startup _ = listen ErrorEvent onError

onError (ErrorNote err) = liftIO $ hPutStrLn stderr ("[ERROR] " ++ err)
onError _ = return ()

{-# LANGUAGE Safe #-}
{-# LANGUAGE LambdaCase #-}

{-|
  Module      : Polar.Core.Log
  Copyright   : (c) 2015 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : non-portable (GHC extensions)

  Logging functions.
-}

module Polar.Core.Log where

import Control.Monad.RWS (MonadIO, liftIO, when)
import System.IO (BufferMode(LineBuffering), stderr, hPutStrLn, hSetBuffering)
import Polar.Types
import Polar.Core.Config

setupLog :: PolarCore ()
setupLog = do
    logCore VERBOSE "setting up logger"
    liftIO (hSetBuffering stderr LineBuffering)

logCore :: Priority -> String -> PolarCore ()
logCore priority msg = getConfig priorityOption "Core" "LogLevel"
    >>= \level -> when (priority >= level) (logIO priority msg)

logIO :: MonadIO m => Priority -> String -> m ()
logIO priority msg = liftIO $ hPutStrLn stderr ('[' : show priority ++ ']' : ' ' : msg)

logFatal :: MonadIO m => String -> m a
logFatal msg = do
    logIO FATAL msg
    liftIO $ ioError (userError msg)

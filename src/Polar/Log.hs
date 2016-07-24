{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
  Module      : Polar.Log
  Copyright   : (c) 2015-2016 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : non-portable (GHC extensions)

  Logging functions.
-}

module Polar.Log where

import Control.Monad.RWS (MonadIO, liftIO, when, tell)
import System.IO (BufferMode(LineBuffering), stderr, hPutStrLn, hSetBuffering)
import Polar.Types
import Polar.Core.Config

class Monad m => LogPolar m where logWrite :: Priority -> String -> m ()

instance LogPolar Core where
    logWrite priority msg = do
        level <- getConfig priorityOption "Core" "LogLevel"
        when (priority >= level) (logIO priority msg)

instance LogPolar Sys where logWrite priority msg = tell [SysLogWriteAction priority msg]

setupLog :: Core ()
setupLog = do
    logWrite VERBOSE "setting up logger"
    liftIO (hSetBuffering stderr LineBuffering)

logFatal :: MonadIO m => String -> m a
logFatal msg = do
    logIO FATAL msg
    liftIO $ ioError (userError msg)

logIO :: MonadIO m => Priority -> String -> m ()
logIO priority msg = liftIO $ hPutStrLn stderr ('[' : show priority ++ ']' : ' ' : msg)

{-# LANGUAGE Safe #-}

{-|
  Module      : Polar.Core.File
  Copyright   : (c) 2015 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : portable

  Functions for managing files synchronously and asynchronously.
-}

module Polar.Core.File (loadFileNow) where

import Control.Monad.RWS (MonadIO, liftIO)
import Control.Exception (IOException, catch)
import {-# SOURCE #-} Polar.Core.Log (logFatal)

loadFileNow :: MonadIO m => FilePath -> m String
loadFileNow path = liftIO (readFile path `catch` handler path)

handler :: MonadIO m => FilePath -> IOException -> m a
handler path err = logFatal ("failed to load file `" ++ path ++ "` (" ++ show err ++ ")")

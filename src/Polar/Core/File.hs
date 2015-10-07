{-# LANGUAGE Safe #-}

{-|
  Module      : Polar.Core.File
  Copyright   : (c) 2015 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : portable

  Functions for managing files synchronously and asynchronously.
-}

module Polar.Core.File where

import Control.Monad.RWS (liftIO)
import Control.Exception (IOException, catch)
import Polar.Types
import {-# SOURCE #-} Polar.Core.Log (logFatal)

loadFileNow :: FilePath -> PolarCore String
loadFileNow path = liftIO (readFile path `catch` handler path)

handler :: FilePath -> IOException -> IO a
handler path err = logFatal ("failed to load file `" ++ path ++ "` (" ++ show err ++ ")")

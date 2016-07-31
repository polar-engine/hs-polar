{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Polar.Log where

import Control.Monad.RWS (MonadIO)
import Polar.Types

class Monad m => LogPolar m where logWrite :: Priority -> String -> m ()
instance LogPolar Core

logFatal :: MonadIO m => String -> m a

{-# LANGUAGE Safe #-}

module Polar.Core.Log where

import Control.Monad.RWS (MonadIO)

logFatal :: MonadIO m => String -> m a

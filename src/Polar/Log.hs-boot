{-# LANGUAGE Safe #-}

module Polar.Log where

import Control.Monad.RWS (MonadIO)

logFatal :: MonadIO m => String -> m a

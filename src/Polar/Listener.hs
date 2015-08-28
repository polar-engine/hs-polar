{-# LANGUAGE FlexibleContexts #-}

module Polar.Listener where

import Data.Maybe (fromMaybe)
import Control.Lens ((<>=), use, at)
import Polar.Types

listen :: MonadPolarState m => Event -> Listener -> m ()
listen event listener = listeners . at event <>= Just [listener]

notify :: Event -> Notification -> PolarIO ()
notify event note = use (listeners . at event) >>= mapM_ ($ note) . fromMaybe []

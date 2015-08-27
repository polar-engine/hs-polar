{-# LANGUAGE LambdaCase #-}

module Polar.Listener where

import Data.Maybe (fromMaybe)
import Control.Lens ((%=), use, at)
import Polar.Types

listen :: Event -> Listener -> PolarIO ()
listen event listener = listeners . at event %= Just . (++ [listener]) . fromMaybe []

notify :: Event -> Notification -> PolarIO ()
notify event note = use (listeners . at event) >>= mapM_ ($ note) . fromMaybe []

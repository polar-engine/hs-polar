{-# LANGUAGE FlexibleContexts #-}

module Polar.Listener where

import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Data.Dynamic (toDyn, fromDynamic)
import Control.Monad (when)
import Control.Lens ((<>=), use, at)
import Polar.Types

listen :: MonadPolarState m => Notification -> Listener -> m ()
listen note listener = listeners . at note <>= Just [listener]

notify :: Typeable a => Notification -> a -> PolarIO ()
notify = notify' 1

notify' :: Typeable a => Int -> Notification -> a -> PolarIO ()
notify' n note x = use (listeners . at note) >>= mapM_ notifyOne . fromMaybe []
  where notifyOne (Listener f) = case fromDynamic (toDyn f) of
            Nothing -> when (n > 0) $ notify' (pred n) ErrorNote ("type mismatch in " ++ show note)
            Just fn -> fn note x

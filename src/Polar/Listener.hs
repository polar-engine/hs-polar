{-# LANGUAGE FlexibleContexts #-}

{-|
  Module      : Polar.Listener
  Copyright   : (c) 2015 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : non-portable (GHC Extensions)

  Helper functions to perform common operations on engine event listeners.
-}

module Polar.Listener (listen, notify, notify') where

import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Data.Dynamic (toDyn, fromDynamic)
import Control.Monad (when)
import Control.Lens ((<>=), use, at)
import Polar.Types

-- |Insert a listener into the engine.
listen :: MonadPolarState m
       => String   -- ^ event name
       -> Listener -- ^ listener
       -> m ()
listen note listener = listeners . at note <>= Just [listener]

-- |Dispatch an event to all listeners.
notify :: Typeable a
       => String -- ^ event name
       -> a      -- ^ argument
       -> PolarIO ()
notify = notify' 1

-- |Dispatch an event to all listeners with a maximum error notification depth.
notify' :: Typeable a
        => Int    -- ^ maximum error notification depth
        -> String -- ^ event name
        -> a      -- ^ argument
        -> PolarIO ()
notify' n note x = use (listeners . at note) >>= mapM_ notifyOne . fromMaybe []
  where notifyOne (Listener f) = case fromDynamic (toDyn f) of
            Nothing -> when (n > 0) $ notify' (pred n) "error" ("type mismatch in " ++ show note)
            Just fn -> fn note x

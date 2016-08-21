{-# LANGUAGE Safe #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
  Module      : Polar.System.Renderer
  Copyright   : (c) 2016 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : non-portable (GHC extensions)

  Generic renderer utility functions.
-}

module Polar.System.Renderer
( submitPrimitive, readRendererMsg
) where

import Data.Hashable
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan
import GHC.Generics
import Polar.Types
import Polar.Storage
import Polar.Unique

data RendererKey = RendererKey deriving Generic
instance Hashable RendererKey

submitPrimitive :: (MonadIO m, StorePolar m) => Primitive -> m Integer
submitPrimitive x = do
    uid <- unique
    chan <- mRetrieveKeyed RendererKey >>= \case
        Nothing -> do
            c <- liftIO (atomically newTChan)
            storeKeyed RendererKey c
            pure c
        Just c  -> pure c
    liftIO $ atomically (writeTChan chan (uid, x))
    pure uid

readRendererMsg :: (MonadIO m, StorePolar m) => m (Maybe (Integer, Primitive))
readRendererMsg = do
    mRetrieveKeyed RendererKey >>= \case
        Nothing   -> pure Nothing
        Just chan -> liftIO $ atomically (tryReadTChan chan)

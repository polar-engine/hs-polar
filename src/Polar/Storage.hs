{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
  Module      : Polar.Storage
  Copyright   : (c) 2016 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : portable

  Engine data storage.
-}

module Polar.Storage
( Proxy(..)
, store, retrieve, forceRetrieve
) where

import Data.Typeable
import Data.Dynamic
import Control.Monad.Trans (MonadIO)
import Polar.Types
import Polar.Log

class Monad m => StorePolar m where
    storeDyn    :: Dynamic -> String -> m ()
    retrieveDyn :: TypeRep -> String -> m (Maybe Dynamic)

instance StorePolar Core where
    storeDyn    dyn k = storage . at (dynTypeRep dyn) . non' _Empty . at k ?= dyn
    retrieveDyn rep k = use (storage . at rep . non' _Empty . at k)

store :: (StorePolar m, Typeable a) => a -> String -> m ()
store x k = storeDyn (toDyn x) k

retrieve :: (StorePolar m, Typeable a) => Proxy a -> String -> m (Maybe a)
retrieve proxy k = maybe Nothing fromDynamic <$> retrieveDyn (typeRep proxy) k

forceRetrieve :: (MonadIO m, StorePolar m, Typeable a) => Proxy a -> String -> m a
forceRetrieve proxy k = maybe (logFatal msg) pure =<< retrieve proxy k
  where msg = "Failed to retrieve value from storage (TypeRep = " ++ show (typeRep proxy) ++ ", Key = " ++ k ++ ")"

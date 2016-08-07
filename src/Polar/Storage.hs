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
, store,      retrieve,      forceRetrieve
, storeNamed, retrieveNamed, forceRetrieveNamed
, retrieveAll
) where

import Data.Maybe (maybeToList)
import Data.Typeable
import Data.Dynamic
import qualified Data.Vector as V
import Control.Monad.Trans (MonadIO)
import Polar.Types
import Polar.Log

class Monad m => StorePolar m where
    storeDyn     :: Dynamic -> m Int
    retrieveDyn  :: TypeRep -> Int -> m (Maybe Dynamic)
    storeName    :: Dynamic -> String -> Int -> m ()
    retrieveName :: TypeRep -> String -> m (Maybe Int)
    retrieveVec  :: TypeRep -> m (V.Vector Dynamic)

instance StorePolar Core where
    storeDyn dyn        = storage . at (dynTypeRep dyn) . non' _Empty . innerDyns %%=
        \v -> (V.length v, v `V.snoc` dyn)
    retrieveDyn rep idx = preuse (storage . at rep . non' _Empty . innerDyns . ix idx)
    storeName dyn k idx = storage . at (dynTypeRep dyn) . non' _Empty . innerNames . at k ?= idx
    retrieveName rep k  = use (storage . at rep . non' _Empty . innerNames . at k)
    retrieveVec rep     = use (storage . at rep . non' _Empty . innerDyns)

store :: (StorePolar m, Typeable a) => a -> m Int
store x = storeDyn (toDyn x)

retrieve :: (StorePolar m, Typeable a) => Proxy a -> Int -> m (Maybe a)
retrieve proxy idx = maybe Nothing fromDynamic <$> retrieveDyn (typeRep proxy) idx

forceRetrieve :: (MonadIO m, StorePolar m, Typeable a) => Proxy a -> Int -> m a
forceRetrieve proxy idx = maybe (logFatal msg) pure =<< retrieve proxy idx
  where msg = "Failed to retrieve value from storage (TypeRep = " ++ show (typeRep proxy) ++ ", Index = " ++ show idx ++ ")"

storeNamed :: (StorePolar m, Typeable a) => a -> String -> m Int
storeNamed x k = do
    i <- store x
    storeName (toDyn x) k i
    pure i

retrieveNamed :: (StorePolar m, Typeable a) => Proxy a -> String -> m (Maybe a)
retrieveNamed proxy k = maybe (pure Nothing) (retrieve proxy) =<< retrieveName (typeRep proxy) k

forceRetrieveNamed :: (MonadIO m, StorePolar m, Typeable a) => Proxy a -> String -> m a
forceRetrieveNamed proxy k = maybe (logFatal msg) (forceRetrieve proxy) =<< retrieveName (typeRep proxy) k
  where msg = "Failed to retrieve value from storage (TypeRep = " ++ show (typeRep proxy) ++ ", Key = " ++ k ++ ")"

retrieveAll :: (StorePolar m, Typeable a) => Proxy a -> m [a]
retrieveAll proxy = foldMap maybeToList . fmap fromDynamic <$> retrieveVec (typeRep proxy)

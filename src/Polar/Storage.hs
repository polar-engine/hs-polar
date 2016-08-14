{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
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
, store, storeKeyed
, mRetrieveP, mRetrieveKeyedP, retrieveP, retrieveKeyedP, retrieveAllP
, mRetrieve,  mRetrieveKeyed,  retrieve,  retrieveKeyed,  retrieveAll
) where

import Data.Maybe (maybeToList)
import Data.Typeable
import Data.Dynamic
import Data.Hashable (Hashable, hash)
import qualified Data.Vector as V
import Control.Monad.Trans (MonadIO)
import Polar.Types
import Polar.Log

-- internal functions

class Monad m => StorePolar m where
    storeDyn     :: Dynamic -> m Int
    retrieveDyn  :: TypeRep -> Int -> m (Maybe Dynamic)
    storeName    :: (Typeable k, Hashable k) => Dynamic -> k -> Int -> m ()
    retrieveName :: (Typeable k, Hashable k) => TypeRep -> k -> m (Maybe Int)
    retrieveVec  :: TypeRep -> m (V.Vector Dynamic)

instance StorePolar Core where
    storeDyn dyn = storage.at (dynTypeRep dyn).non' _Empty.dyns %%=
        \v -> (V.length v, v `V.snoc` dyn)

    retrieveDyn rep idx = preuse (storage.at rep.non' _Empty.dyns.ix idx)

    storeName :: forall k. (Typeable k, Hashable k) => Dynamic -> k -> Int -> Core ()
    storeName dyn key idx = storage . at (dynTypeRep dyn) . non' _Empty
                          . keys    . at (typeRep p)      . non' _Empty
                          . at (hash key) ?= idx
      where p = Proxy :: Proxy k

    retrieveName :: forall k. (Typeable k, Hashable k) => TypeRep -> k -> Core (Maybe Int)
    retrieveName rep key = use $ storage . at rep         . non' _Empty
                               . keys    . at (typeRep p) . non' _Empty
                               . at (hash key)
      where p = Proxy :: Proxy k

    retrieveVec rep = use (storage.at rep.non' _Empty.dyns)

-- store functions

store :: (StorePolar m, Typeable a) => a -> m Int
store x = storeDyn (toDyn x)

storeKeyed :: (StorePolar m, Typeable a, Typeable k, Hashable k)
           => a -> k -> m Int
storeKeyed x key = do
    i <- store x
    storeName (toDyn x) key i
    pure i

-- explicit proxy functions

mRetrieveP :: (StorePolar m, Typeable a) => Proxy a -> Int -> m (Maybe a)
mRetrieveP proxy idx = maybe Nothing fromDynamic <$> retrieveDyn (typeRep proxy) idx

mRetrieveKeyedP :: (StorePolar m, Typeable a, Typeable k, Hashable k)
                => Proxy a -> k -> m (Maybe a)
mRetrieveKeyedP proxy key = maybe (pure Nothing) (mRetrieveP proxy) =<< retrieveName (typeRep proxy) key

retrieveP :: (MonadIO m, StorePolar m, Typeable a) => Proxy a -> Int -> m a
retrieveP proxy idx = maybe (logFatal msg) pure =<< mRetrieveP proxy idx
  where msg = "Failed to retrieve value from storage (TypeRep = " ++ show (typeRep proxy) ++ ", Index = " ++ show idx ++ ")"

retrieveKeyedP :: (MonadIO m, StorePolar m, Typeable a, Typeable k, Hashable k)
               => Proxy a -> k -> m a
retrieveKeyedP proxy key = maybe (logFatal msg) (retrieveP proxy) =<< retrieveName (typeRep proxy) key
  where msg = "Failed to retrieve value from storage (TypeRep = " ++ show (typeRep proxy) ++ ", Key = " ++ show (hash key) ++ ")"

retrieveAllP :: (StorePolar m, Typeable a) => Proxy a -> m [a]
retrieveAllP proxy = foldMap maybeToList . fmap fromDynamic <$> retrieveVec (typeRep proxy)

-- implicit inferred functions

mRetrieve :: (StorePolar m, Typeable a) => Int -> m (Maybe a)
mRetrieve = mRetrieveP Proxy

mRetrieveKeyed :: (StorePolar m, Typeable a, Typeable k, Hashable k)
               => k -> m (Maybe a)
mRetrieveKeyed = mRetrieveKeyedP Proxy

retrieve :: (MonadIO m, StorePolar m, Typeable a) => Int -> m a
retrieve = retrieveP Proxy

retrieveKeyed :: (MonadIO m, StorePolar m, Typeable a, Typeable k, Hashable k)
              => k -> m a
retrieveKeyed = retrieveKeyedP Proxy

retrieveAll :: (StorePolar m, Typeable a) => m [a]
retrieveAll = retrieveAllP Proxy

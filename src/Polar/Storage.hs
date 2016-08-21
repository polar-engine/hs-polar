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
  Portability : non-portable (GHC extensions)

  Engine data storage.
-}

module Polar.Storage
( Proxy(..)
, StorePolar
, store, storeKeyed
, mRetrieveP, mRetrieveKeyedP, retrieveP, retrieveKeyedP, retrieveAllP
, mRetrieve,  mRetrieveKeyed,  retrieve,  retrieveKeyed,  retrieveAll
, storeMsg, storeKeyedMsg
, storeDyn, retrieveDyn, storeKey, retrieveKey
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
    storeKey     :: TypeRep -> Int -> TypeRep -> Int -> m ()
    retrieveKey  :: TypeRep -> Int -> TypeRep -> m (Maybe Int)
    retrieveVec  :: TypeRep -> m (V.Vector Dynamic)

instance StorePolar Core where
    storeDyn dyn = storage.at (dynTypeRep dyn).non' _Empty.dyns %%=
        \v -> (V.length v, v `V.snoc` dyn)

    retrieveDyn rep idx = preuse (storage.at rep.non' _Empty.dyns.ix idx)

    storeKey :: TypeRep -> Int -> TypeRep -> Int -> Core ()
    storeKey keyRep hsh rep idx = storage . at rep    . non' _Empty
                                . keys    . at keyRep . non' _Empty
                                . at hsh ?= idx

    retrieveKey :: TypeRep -> Int -> TypeRep -> Core (Maybe Int)
    retrieveKey keyRep hsh rep = use $ storage . at rep    . non' _Empty
                                     . keys    . at keyRep . non' _Empty
                                     . at hsh

    retrieveVec rep = use (storage.at rep.non' _Empty.dyns)

-- store functions

store :: (StorePolar m, Typeable a) => a -> m Int
store x = storeDyn (toDyn x)

storeKeyed :: forall m a k. (StorePolar m, Typeable a, Typeable k, Hashable k)
           => k -> a -> m Int
storeKeyed key x = do
    i <- store x
    storeKey (typeRep (Proxy :: Proxy k)) (hash key)
             (typeRep (Proxy :: Proxy a)) i
    pure i

-- explicit proxy functions

mRetrieveP :: (StorePolar m, Typeable a) => Proxy a -> Int -> m (Maybe a)
mRetrieveP proxy idx = maybe Nothing fromDynamic <$> retrieveDyn (typeRep proxy) idx

mRetrieveKeyedP :: forall m a k. (StorePolar m, Typeable a, Typeable k, Hashable k)
                => k -> Proxy a -> m (Maybe a)
mRetrieveKeyedP key proxy = do
    mVal <- retrieveKey (typeRep (Proxy :: Proxy k)) (hash key) (typeRep proxy)
    maybe (pure Nothing) (mRetrieveP proxy) mVal

retrieveP :: (MonadIO m, StorePolar m, Typeable a) => Proxy a -> Int -> m a
retrieveP proxy idx = maybe (logFatal msg) pure =<< mRetrieveP proxy idx
  where msg = "Failed to retrieve value from storage (TypeRep = " ++ show (typeRep proxy) ++ ", Index = " ++ show idx ++ ")"

retrieveKeyedP :: forall m a k. (MonadIO m, StorePolar m, Typeable a, Typeable k, Hashable k)
               => k -> Proxy a -> m a
retrieveKeyedP key proxy = do
    mVal <- retrieveKey (typeRep (Proxy :: Proxy k)) (hash key) (typeRep proxy)
    maybe (logFatal msg) (retrieveP proxy) mVal
  where msg = "Failed to retrieve value from storage (TypeRep = " ++ show (typeRep proxy) ++ ", Key = " ++ show (hash key) ++ ")"

retrieveAllP :: (StorePolar m, Typeable a) => Proxy a -> m [a]
retrieveAllP proxy = foldMap maybeToList . fmap fromDynamic <$> retrieveVec (typeRep proxy)

-- implicit inferred functions

mRetrieve :: (StorePolar m, Typeable a) => Int -> m (Maybe a)
mRetrieve = mRetrieveP Proxy

mRetrieveKeyed :: (StorePolar m, Typeable a, Typeable k, Hashable k)
               => k -> m (Maybe a)
mRetrieveKeyed key = mRetrieveKeyedP key Proxy

retrieve :: (MonadIO m, StorePolar m, Typeable a) => Int -> m a
retrieve = retrieveP Proxy

retrieveKeyed :: (MonadIO m, StorePolar m, Typeable a, Typeable k, Hashable k)
              => k -> m a
retrieveKeyed key = retrieveKeyedP key Proxy

retrieveAll :: (StorePolar m, Typeable a) => m [a]
retrieveAll = retrieveAllP Proxy

-- msg functions

storeMsg :: Typeable a => a -> CoreMsg
storeMsg x = CoreStoreMsg Nothing (toDyn x)

storeKeyedMsg :: forall a k. (Typeable a, Typeable k, Hashable k) => k -> a -> CoreMsg
storeKeyedMsg key x = CoreStoreMsg (Just (rep, hash key)) (toDyn x)
  where rep = typeRep (Proxy :: Proxy k)

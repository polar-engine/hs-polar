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
, store, storeNamed
, mRetrieveP, mRetrieveNamedP, retrieveP, retrieveNamedP, retrieveAllP
, mRetrieve, mRetrieveNamed, retrieve, retrieveNamed, retrieveAll
) where

import Data.Maybe (maybeToList)
import Data.Typeable
import Data.Dynamic
import qualified Data.Vector as V
import Control.Monad.Trans (MonadIO)
import Polar.Types
import Polar.Log

-- internal functions

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

-- store functions

store :: (StorePolar m, Typeable a) => a -> m Int
store x = storeDyn (toDyn x)

storeNamed :: (StorePolar m, Typeable a) => a -> String -> m Int
storeNamed x k = do
    i <- store x
    storeName (toDyn x) k i
    pure i

-- explicit proxy functions

mRetrieveP :: (StorePolar m, Typeable a) => Proxy a -> Int -> m (Maybe a)
mRetrieveP proxy idx = maybe Nothing fromDynamic <$> retrieveDyn (typeRep proxy) idx

mRetrieveNamedP :: (StorePolar m, Typeable a) => Proxy a -> String -> m (Maybe a)
mRetrieveNamedP proxy k = maybe (pure Nothing) (mRetrieveP proxy) =<< retrieveName (typeRep proxy) k

retrieveP :: (MonadIO m, StorePolar m, Typeable a) => Proxy a -> Int -> m a
retrieveP proxy idx = maybe (logFatal msg) pure =<< mRetrieveP proxy idx
  where msg = "Failed to retrieve value from storage (TypeRep = " ++ show (typeRep proxy) ++ ", Index = " ++ show idx ++ ")"

retrieveNamedP :: (MonadIO m, StorePolar m, Typeable a) => Proxy a -> String -> m a
retrieveNamedP proxy k = maybe (logFatal msg) (retrieveP proxy) =<< retrieveName (typeRep proxy) k
  where msg = "Failed to retrieve value from storage (TypeRep = " ++ show (typeRep proxy) ++ ", Key = " ++ k ++ ")"

retrieveAllP :: (StorePolar m, Typeable a) => Proxy a -> m [a]
retrieveAllP proxy = foldMap maybeToList . fmap fromDynamic <$> retrieveVec (typeRep proxy)

-- implicit inferred functions

mRetrieve :: (StorePolar m, Typeable a) => Int -> m (Maybe a)
mRetrieve = mRetrieveP Proxy

mRetrieveNamed :: (StorePolar m, Typeable a) => String -> m (Maybe a)
mRetrieveNamed = mRetrieveNamedP Proxy

retrieve :: (MonadIO m, StorePolar m, Typeable a) => Int -> m a
retrieve = retrieveP Proxy

retrieveNamed :: (MonadIO m, StorePolar m, Typeable a) => String -> m a
retrieveNamed = retrieveNamedP Proxy

retrieveAll :: (StorePolar m, Typeable a) => m [a]
retrieveAll = retrieveAllP Proxy

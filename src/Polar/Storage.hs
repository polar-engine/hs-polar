{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
  Module      : Polar.Storage
  Copyright   : (c) 2016 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : portable

  Engine data storage.
-}

module Polar.Storage where

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

data As a = As

class RetrieveFn t      where retrieve      :: t
class ForceRetrieveFn t where forceRetrieve :: t

instance (StorePolar m, Typeable a) => RetrieveFn (As a -> String -> m (Maybe a)) where
    retrieve _ k = maybe Nothing fromDynamic <$> retrieveDyn rep k
      where rep = typeRep (Proxy :: Proxy a)

instance (MonadIO m, StorePolar m, Typeable a) => ForceRetrieveFn (As a -> String -> m a) where
    forceRetrieve as k = maybe (logFatal msg) pure =<< retrieve as k
      where rep = typeRep (Proxy :: Proxy a)
            msg = "Failed to retrieve value from storage (TypeRep = " ++ show rep ++ ", Key = " ++ k ++ ")"

instance (StorePolar m, Typeable a) => RetrieveFn (String -> m (Maybe a))
    where retrieve = retrieve (As :: As a)

instance (MonadIO m, StorePolar m, Typeable a) => ForceRetrieveFn (String -> m a)
    where forceRetrieve = forceRetrieve (As :: As a)

store :: (StorePolar m, Typeable a) => a -> String -> m ()
store x k = storeDyn (toDyn x) k

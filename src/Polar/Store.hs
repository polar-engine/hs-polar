{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
  Module      : Polar.Store
  Copyright   : (c) 2016 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : portable

  Engine data store.
-}

module Polar.Store where

import Data.Typeable
import Data.Dynamic
import Control.Monad.Trans (MonadIO)
import Polar.Types
import Polar.Log

class Monad m => StorePolar m where
    storeDyn    :: Dynamic -> String -> m ()
    retrieveDyn :: TypeRep -> String -> m Dynamic

instance StorePolar Core where
    storeDyn    dyn k = pure ()
    retrieveDyn rep k = pure undefined

store :: (StorePolar m, Typeable a) => a -> String -> m ()
store x k = storeDyn (toDyn x) k

retrieve :: forall m a. (StorePolar m, Typeable a) => String -> m (Maybe a)
retrieve k = fromDynamic <$> retrieveDyn (typeRep (Proxy :: Proxy a)) k

forceRetrieve :: forall m a. (MonadIO m, StorePolar m, Typeable a) => String -> m a
forceRetrieve k = retrieve k >>= \case
    Nothing -> logFatal ("Failed to retrieve value from store (TypeRep=" ++ show rep ++ ",Key=" ++ k ++ ")")
    Just x  -> pure x
  where rep = typeRep (Proxy :: Proxy a)

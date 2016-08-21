{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
  Module      : Polar.Unique
  Copyright   : (c) 2016 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : non-portable (GHC extensions)

  Functions for generating unique values.
-}

module Polar.Unique (unique) where

import Data.Typeable
import Data.Hashable
import GHC.Generics
import Polar.Storage

data UniqueKey = UniqueKey deriving Generic
instance Hashable UniqueKey

unique :: (StorePolar m, Enum a, Typeable a) => m a
unique = do
    x <- maybe (toEnum 0) succ <$> mRetrieveKeyed UniqueKey
    storeKeyed UniqueKey x
    pure x

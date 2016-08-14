{-# LANGUAGE Trustworthy #-}

{-|
  Module      : Polar.Types.Storage
  Copyright   : (c) 2016 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : portable

  Engine data storage types.
-}

module Polar.Types.Storage where

import Data.Typeable
import Data.Dynamic
import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as M

type Storage = M.HashMap TypeRep VectorStorage

data VectorStorage = VectorStorage
    { _vectorStorageDyns :: V.Vector Dynamic
    , _vectorStorageKeys :: M.HashMap TypeRep KeyStorage
    }

-- key hash => index
type KeyStorage = M.HashMap Int Int

defaultStorage :: Storage
defaultStorage = M.empty

defaultVectorStorage :: VectorStorage
defaultVectorStorage = VectorStorage
    { _vectorStorageDyns = V.empty
    , _vectorStorageKeys = M.empty
    }

vectorStorageNull :: VectorStorage -> Bool
vectorStorageNull s = V.null (_vectorStorageDyns s)

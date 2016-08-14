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

data InnerStorage = InnerStorage
    { _innerStorageDyns  :: V.Vector Dynamic
    , _innerStorageNames :: M.HashMap String Int
    }

defaultInnerStorage :: InnerStorage
defaultInnerStorage = InnerStorage
    { _innerStorageDyns  = V.empty
    , _innerStorageNames = M.empty
    }

innerStorageNull :: InnerStorage -> Bool
innerStorageNull s = V.null (_innerStorageDyns s)

type Storage = M.HashMap TypeRep InnerStorage

defaultStorage :: Storage
defaultStorage = M.empty

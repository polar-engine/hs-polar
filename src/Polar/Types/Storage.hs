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
import Control.Lens (Lens, Field1, Field2, _1, _2)

type InnerStorage = (V.Vector Dynamic, M.HashMap String Int)

innerDyns :: Field1 s t a b => Lens s t a b
innerDyns = _1

innerNames :: Field2 s t a b => Lens s t a b
innerNames = _2

type Storage = M.HashMap TypeRep InnerStorage

defaultStorage :: Storage
defaultStorage = M.empty

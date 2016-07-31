{-# LANGUAGE Safe #-}

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
import qualified Data.HashMap.Lazy as M

type Storage = M.HashMap TypeRep (M.HashMap String Dynamic)

defaultStorage :: Storage
defaultStorage = M.empty

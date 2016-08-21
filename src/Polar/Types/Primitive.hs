{-# LANGUAGE Safe #-}

{-|
  Module      : Polar.Types.Primitive
  Copyright   : (c) 2016 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : portable

  Renderer types.
-}

module Polar.Types.Primitive (Primitive) where

import Foreign.C.Types (CFloat)

type Primitive = [CFloat]

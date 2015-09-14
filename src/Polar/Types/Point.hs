{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

{-|
  Module      : Polar.Types.Point
  Copyright   : (c) 2015 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : non-portable (GHC extensions)

  This module defines the point type.
-}

module Polar.Types.Point where

-- |Representation of a 4-component point with x, y, z, and weight.
data Point a = Point { _pointX :: a -- ^ x component
                     , _pointY :: a -- ^ y component
                     , _pointZ :: a -- ^ z component
                     , _pointW :: a -- ^ weight component
                     } deriving (Eq, Show, Functor, Foldable)

-- |Map numeric operations over all components.
instance Num a => Num (Point a) where
    p1 + p2 = foldr (+) (fromInteger 0) [p1, p2]
    p1 * p2 = foldr (*) (fromInteger 0) [p1, p2]
    p1 - p2 = foldl (-) (fromInteger 0) [p1, p2]
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger x = Point i i i i where i = fromInteger x

-- |Default value for 'Point'
defaultPoint :: Num a => Point a
defaultPoint = fromInteger 0

-- TODO: make HasMagnitude somehow using Control.Lens.TH
-- |Calculate the scalar magnitude of a 'Point'
magnitude :: RealFloat a
          => Point a -- ^ point
          -> a
magnitude (Point x y z w) = sqrt (x * x + y * y + z * z + w * w)

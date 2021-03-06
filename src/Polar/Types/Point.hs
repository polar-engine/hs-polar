{-# LANGUAGE Trustworthy #-}
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

import Control.Lens.Lens (Lens')

-- |Representation of a 4-component point with x, y, z, and weight.
data Point a = Point { _pointX :: a -- ^ x component
                     , _pointY :: a -- ^ y component
                     , _pointZ :: a -- ^ z component
                     , _pointW :: a -- ^ weight component
                     } deriving (Eq, Show, Functor, Foldable)

-- |Perform numeric operations on x, y, and z components.
instance Num a => Num (Point a) where
    (Point x1 y1 z1 w1) + (Point x2 y2 z2 w2) = Point (x1 * w1 + x2 * w2) (y1 * w1 + y2 * w2) (z1 * w1 + z2 * w2) 1
    (Point x1 y1 z1 w1) * (Point x2 y2 z2 w2) = Point (x1 * w1 * x2 * w2) (y1 * w1 * y2 * w2) (z1 * w1 * z2 * w2) 1
    p1 - p2 = p1 + (negate p2)
    negate (Point x y z w) = Point x y z (negate w)
    abs = fmap abs
    signum = fmap signum
    fromInteger x = let i = fromInteger x in Point i i i 1

-- |Default value for 'Point'.
defaultPoint :: Num a => Point a
defaultPoint = fromInteger 0

-- |Lens into the scalar magnitude of a 'Point'.
magnitude :: (Eq a, Floating a) => Lens' (Point a) a
magnitude f (Point x y z 1) = Point x y z <$> f (sqrt (x * x + y * y + z * z))
magnitude f (Point x y z w) = magnitude f (Point (x * w) (y * w) (z * w) 1)

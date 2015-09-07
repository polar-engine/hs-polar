{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module Polar.Types.Point where

data Point a = Point { _pointX :: a
                     , _pointY :: a
                     , _pointZ :: a
                     , _pointW :: a
                     } deriving (Eq, Show, Functor, Foldable)

instance Num a => Num (Point a) where
    p1 + p2 = foldr (+) (fromInteger 0) [p1, p2]
    p1 * p2 = foldr (*) (fromInteger 0) [p1, p2]
    p1 - p2 = foldl (-) (fromInteger 0) [p1, p2]
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger x = Point i i i i where i = fromInteger x

defaultPoint :: Num a => Point a
defaultPoint = fromInteger 0

-- TODO: make HasMagnitude somehow using Control.Lens.TH
magnitude :: RealFloat a => Point a -> a
magnitude (Point x y z w) = sqrt (x * x + y * y + z * z + w * w)

module Polar.Types.Rectangle where

import Polar.Types.Point2

data Rectangle a = Rectangle
    { origin    :: Point2 a
    , size      :: Point2 a
    } deriving (Show)

instance Eq a => Eq (Rectangle a) where
    Rectangle o1 s1 == Rectangle o2 s2 = o1 == s1 && o2 == s2
    r1 /= r2 = not (r1 == r2)

mapOrigin :: (Point2 a -> Point2 a) -> Rectangle a -> Rectangle a
mapOrigin f v = v {origin = f (origin v)}

mapSize :: (Point2 a -> Point2 a) -> Rectangle a -> Rectangle a
mapSize f v = v {size = f (size v)}

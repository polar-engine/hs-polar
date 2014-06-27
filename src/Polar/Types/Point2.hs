module Polar.Types.Point2 where

import Prelude hiding (length)

data Point2 a = Point2
    { x :: a
    , y :: a
    } deriving (Show)

instance Eq a => Eq (Point2 a) where
    Point2 x1 y1 == Point2 x2 y2 = x1 == x2 && y1 == y2
    p1 /= p2 = not (p1 == p2)

instance Num a => Num (Point2 a) where
    Point2 x1 y1 + Point2 x2 y2 = Point2 (x1 + x2) (y1 + y2)
    Point2 x1 y1 * Point2 x2 y2 = Point2 (x1 * x2) (y1 * y2)
    Point2 x1 y1 - Point2 x2 y2 = Point2 (x1 - x2) (y1 - y2)
    negate (Point2 x y) = Point2 (negate x) (negate y)
    abs (Point2 x y) = Point2 (abs x) (abs y)
    signum (Point2 x y) = Point2 (signum x) (signum y)
    fromInteger i = Point2 i' i'
      where i' = fromInteger i

mapX :: (a -> a) -> Point2 a -> Point2 a
mapX f v = v {x = f (x v)}

mapY :: (a -> a) -> Point2 a -> Point2 a
mapY f v = v {y = f (y v)}

length :: RealFloat a => Point2 a -> a
length (Point2 x y) = x * cos (atan2 y x)

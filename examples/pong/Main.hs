module Main where

import Data.Typeable
import Polar

-- Polar

(<->) :: (Typeable o, Typeable c) => o -> c -> Core ()
_ <-> _ = undefined

data Sprite = Sprite String
data Phys = StaticPhys | BouncePhys Rational
data Bounds = SpriteBounds | ViewportBounds | InvertedBounds Bounds

data Viewport = Viewport
data Ball = Ball
data Paddle = LeftPaddle | RightPaddle

logic = do
    Viewport <-> StaticPhys
    Viewport <-> InvertedBounds ViewportBounds
    Ball <-> BouncePhys 1
    Ball <-> SpriteBounds
    Ball <-> Sprite "ball"
    LeftPaddle <-> StaticPhys
    LeftPaddle <-> SpriteBounds
    LeftPaddle <-> Sprite "paddle"
    RightPaddle <-> StaticPhys
    RightPaddle <-> SpriteBounds
    RightPaddle <-> Sprite "paddle"
    --on (Ball `CollideX` Viewport) reset

main = run $ defaultEngine

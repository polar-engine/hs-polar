module Main where

import Polar

data Ball = Ball
data Paddle = LeftPaddle | RightPaddle

logic = do
    Ball        <=> [SpriteBoxBounds,   BouncePhys, CenterAnchor, Sprite "ball"]
    LeftPaddle  <=> [SpriteYAxisBounds, StaticPhys, LeftAnchor,   Sprite "paddle"]
    RightPaddle <=> [SpriteYAxisBounds, StaticPhys, RightAnchor,  Sprite "paddle"]

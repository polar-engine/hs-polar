module Polar.Types.Color where

import qualified Graphics.Rendering.OpenGL as GL (Color4(..))

data Color = Color3 Double Double Double
           | Color4 Double Double Double Double

colorToGL :: (Fractional a) => Color -> GL.Color4 a
colorToGL (Color3 r g b) = colorToGL (Color4 r g b 1)
colorToGL (Color4 r g b a) = GL.Color4
    (fromRational (toRational r))
    (fromRational (toRational g))
    (fromRational (toRational b))
    (fromRational (toRational a))

noColor         = Color4 0 0 0 0
blackColor      = Color3 0 0 0
whiteColor      = Color3 1 1 1
redColor        = Color3 1 0 0
greenColor      = Color3 0 1 0
blueColor       = Color3 0 0 1
navyBlueColor   = Color3 0.02 0.05 0.1

module Polar.Types.Color where

data Color = Color3 Double Double Double
           | Color4 Double Double Double Double

noColor         = Color4 0 0 0 0
blackColor      = Color3 0 0 0
whiteColor      = Color3 1 1 1
redColor        = Color3 1 0 0
greenColor      = Color3 0 1 0
blueColor       = Color3 0 0 1
navyBlueColor   = Color3 0.02 0.05 0.1

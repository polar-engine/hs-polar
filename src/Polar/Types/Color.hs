{-# LANGUAGE Safe #-}

{-|
  Module      : Polar.Types.Color
  Copyright   : (c) 2015 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : portable

  This module defines color types.
-}

module Polar.Types.Color where

-- |Representation of a color.
data Color = Color3 Double Double Double        -- ^ red green blue
           | Color4 Double Double Double Double -- ^ red green blue alpha

noColor :: Color
noColor = Color4 0 0 0 0

blackColor :: Color
blackColor = Color3 0 0 0

whiteColor :: Color
whiteColor = Color3 1 1 1

redColor :: Color
redColor = Color3 1 0 0

greenColor :: Color
greenColor = Color3 0 1 0

blueColor :: Color
blueColor = Color3 0 0 1

navyBlueColor :: Color
navyBlueColor = Color3 0.02 0.05 0.1

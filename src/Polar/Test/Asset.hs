module Polar.Test.Asset where

import Test.HUnit
import qualified Polar.Test.Asset.Shader as Shader

tests = TestList [ TestLabel "Shader" Shader.tests
                 ]

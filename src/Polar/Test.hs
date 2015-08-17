module Polar.Test where

import Test.HUnit
import qualified Polar.Test.Asset as Asset

tests = TestList [ TestLabel "Asset" Asset.tests
                 ]

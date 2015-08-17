module Main where

import Test.HUnit
import qualified Polar.Test

tests = TestList [ TestLabel "Polar" Polar.Test.tests
                 ]

main = runTestTT tests

module Main where

import System.Exit (exitFailure, exitSuccess)
import qualified Test.HUnit as HU
import qualified Polar.Test

main :: IO ()
main = do
    (HU.Counts _ _ errors failures) <- HU.runTestTT (HU.TestLabel "Polar" Polar.Test.tests)
    if errors > 0 || failures > 0 then exitFailure else exitSuccess

module Main where

import Polar

main :: IO ()
main = run $ defaultEngine
    & sysTicks .~ [logWrite DEBUG "hello world"]
--    & systems  .~ [hello]

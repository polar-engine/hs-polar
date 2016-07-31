module Main where

import Polar

hello :: System
hello = defaultSystem "Hello"
    & tick .~ exit

main :: IO ()
main = run $ defaultEngine
    & sysTicks .~ [logWrite DEBUG "hello world"]
    & systems  .~ [hello]

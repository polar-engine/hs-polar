module Main where

import Polar

main :: IO ()
main = run [logWrite DEBUG "hello world", exit] []

module Main where

import Polar

hello :: System
hello = defaultSystem "Hello"
    & tick .~ logWrite DEBUG "Hello!"

quitter :: System
quitter = defaultSystem "Quitter"
    & tick .~ exit

main :: IO ()
main = run $ defaultEngine
    & systems .~ [hello, quitter]

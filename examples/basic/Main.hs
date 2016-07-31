module Main where

import Polar

hello :: System
hello = defaultSystem "Hello"
    & startup  .~ logWrite NOTICE "Hello!"
    & shutdown .~ logWrite NOTICE "Goodbye... :("

quitter :: System
quitter = defaultSystem "Quitter"
    & tick .~ exit

main :: IO ()
main = run $ defaultEngine
    & systems .~ [hello, quitter]

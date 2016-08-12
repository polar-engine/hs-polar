module Main where

import Polar
import Polar.System.Renderer.OpenGL_3_2

hello :: System
hello = defaultSystem "Hello"
    & startup  .~ logWrite NOTICE "Hello!"
    & shutdown .~ logWrite NOTICE "Goodbye... :("

main :: IO ()
main = run $ defaultEngine
    & systems .~ [hello, renderer]

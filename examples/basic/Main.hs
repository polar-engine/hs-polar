module Main where

import Control.Monad.RWS (void, tell)
import Polar
import Polar.System.Renderer
import Polar.System.Renderer.OpenGL_3_2

hello :: System
hello = defaultSystem "Hello"
    & startup  .~ logWrite NOTICE "Hello!"
    & shutdown .~ logWrite NOTICE "Goodbye... :("

prim :: System
prim = defaultSystem "Primitive Submitter"
    & startup .~ tell [SysCoreAction (void $ submitPrimitive ())]

main :: IO ()
main = run $ defaultEngine
    & systems .~ [hello, prim, renderer]

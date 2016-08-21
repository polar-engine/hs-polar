module Main where

import Control.Monad.RWS (void, tell)
import Polar
import Polar.System.Renderer
import Polar.System.Renderer.OpenGL_3_2

hello :: System
hello = defaultSystem "Hello"
    & startup  .~ logWrite NOTICE "Hello!"
    & shutdown .~ logWrite NOTICE "Goodbye... :("

primSubmitter :: System
primSubmitter = defaultSystem "Primitive Submitter"
    & startup .~ tell [SysCoreAction (void $ submitPrimitive prim)]

prim :: Primitive
prim = [ -1, -1, 0
       ,  1, -1, 0
       ,  0,  1, 0
       ]

main :: IO ()
main = run $ defaultEngine
    & systems .~ [hello, primSubmitter, renderer]

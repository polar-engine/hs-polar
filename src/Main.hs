module Main where

import Control.Lens ((.~))
import Polar (run)
import Polar.Types
import Polar.Listener
import qualified Polar.Renderer.OpenGL_3_2 as Renderer

main :: IO ()
main = run engine

engine :: Engine
engine = startup .~ listen StartupEvent Renderer.startup $ defaultEngine

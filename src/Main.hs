module Main where

import Polar (run)
import Polar.Types
import Polar.Listener
import qualified Polar.Renderer.OpenGL_3_2 as Renderer

main :: IO ()
main = run defaultEngine { engineStartup = startup }

startup :: PolarIO ()
startup = listen StartupEvent Renderer.startup

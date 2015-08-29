module Main where

import Control.Lens ((.~))
import Polar (run)
import Polar.Types
import qualified Polar.Logger as Logger
import qualified Polar.Renderer.OpenGL_3_2 as Renderer

main :: IO ()
main = run engine

engine :: Engine
engine = startup .~ [ Listener Logger.startup
                    , Listener Renderer.startup
                    ] $ defaultEngine

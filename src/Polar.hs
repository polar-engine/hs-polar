module Polar (run) where

import Control.Monad (unless)
import System.IO (stderr, hPutStrLn)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Polar.Assets.Manager
import qualified Polar.Types.Options as O

import Control.Monad.Trans.State
import Polar.Types.Engine (defaultEngine)
import qualified Polar.Engine

errorCB :: GLFW.ErrorCallback
errorCB _ desc = hPutStrLn stderr desc

run :: O.Options -> IO ()
run opts = do
    GLFW.setErrorCallback (Just errorCB)
    win <- evalStateT Polar.Engine.run defaultEngine
    setup opts win
    loop opts win
    GLFW.terminate

setup :: O.Options -> GLFW.Window -> IO ()
setup opts win = do
    GLFW.setKeyCallback win (O.keyCB opts)
    requireAsset (O.vertexShader opts)
    return ()

loop :: O.Options -> GLFW.Window -> IO ()
loop opts win = GLFW.windowShouldClose win >>= \result -> unless result $ do
    GL.clear [GL.ColorBuffer]
    GLFW.swapBuffers win
    GLFW.pollEvents
    loop opts win

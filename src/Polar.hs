module Polar (run) where

import Control.Monad (unless)
import System.IO (stdout, stderr, hPutStrLn, hSetBuffering, BufferMode(NoBuffering))
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Polar.Assets.Manager
import Polar.Types.Color (colorToGL)
import qualified Polar.Types.Options as O

import Control.Monad.Trans.State
import Polar.Types.Engine (defaultEngine)
import qualified Polar.Engine

errorCB :: GLFW.ErrorCallback
errorCB _ desc = hPutStrLn stderr desc

run :: O.Options -> IO ()
run opts = do
    evalStateT Polar.Engine.run defaultEngine
    GLFW.setErrorCallback (Just errorCB)
    result <- GLFW.init
    unless result $ (fail "GLFW.init")

    GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 2)
    GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True)
    GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)

    maybeWin <- GLFW.createWindow width height (O.title opts) Nothing Nothing
    case maybeWin of
        Nothing -> GLFW.terminate >> fail "GLFW.createWindow"
        Just win -> do
            GLFW.makeContextCurrent maybeWin
            setup opts win
            loop opts win
            GLFW.terminate
  where (width, height) = O.dimensions opts

setup :: O.Options -> GLFW.Window -> IO ()
setup opts win = do
    hSetBuffering stdout NoBuffering
    GLFW.swapInterval (O.swapInterval opts)
    GLFW.setKeyCallback win (O.keyCB opts)
    GL.clearColor $= colorToGL (O.clearColor opts)
    vs <- requireAsset (O.vertexShader opts)
    return ()

loop :: O.Options -> GLFW.Window -> IO ()
loop opts win = GLFW.windowShouldClose win >>= \result -> unless result $ do
    GL.clear [GL.ColorBuffer]
    GLFW.swapBuffers win
    GLFW.pollEvents
    loop opts win

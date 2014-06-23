module Polar
( run
) where

import Control.Monad (unless)
import System.IO (stderr, hPutStrLn)
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

run :: (Int, Int) -> IO ()
run (width, height) = do
    GLFW.setErrorCallback (Just errorCB)
    result <- GLFW.init
    unless result $ (fail "GLFW.init")

    GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 2)
    GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True)
    GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)

    maybeWin <- GLFW.createWindow 1024 576 "Game" Nothing Nothing
    case maybeWin of
        Nothing -> GLFW.terminate >> fail "GLFW.createWindow"
        Just win -> do
            GLFW.makeContextCurrent maybeWin
            setup win
            loop win
            GLFW.terminate

setup :: GLFW.Window -> IO ()
setup win = do
    GLFW.swapInterval 1
    GLFW.setKeyCallback win (Just keyCB)
    GL.clearColor $= GL.Color4 0.02 0.05 0.1 0

errorCB :: GLFW.ErrorCallback
errorCB _ desc = hPutStrLn stderr desc

keyCB :: GLFW.KeyCallback
keyCB win key scancode action mods = return ()

loop :: GLFW.Window -> IO ()
loop win = GLFW.windowShouldClose win >>= \result -> unless result $ do
    GL.clear [GL.ColorBuffer]
    GLFW.swapBuffers win
    GLFW.pollEvents
    loop win

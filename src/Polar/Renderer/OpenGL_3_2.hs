{-# LANGUAGE LambdaCase #-}

module Polar.Renderer.OpenGL_3_2 where

import Control.Monad (unless)
import Control.Monad.State (liftIO)
import System.IO (stderr, hPutStrLn)
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Polar.Types
import Polar.Control
import Polar.Listener

startup :: Listener
startup _ = do
    win <- liftIO $ setupWindow (Box (Point 50) (Point2 640 360)) "Game"
    listen TickEvent (tick win)
    listen ShutdownEvent (shutdown win)

tick :: GLFW.Window -> Notification -> PolarIO ()
tick win _ = liftIO (GLFW.windowShouldClose win) >>= \case
    True  -> exit
    False -> liftIO $ do
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        GLFW.swapBuffers win
        GLFW.pollEvents

shutdown :: GLFW.Window -> Listener
shutdown win _ = liftIO (destroyWindow win)

setupWindow :: Box Int -> String -> IO GLFW.Window
setupWindow (Box origin size) title = do
    GLFW.setErrorCallback (Just errorCB)
    GLFW.init >>= \succeeded -> unless succeeded (fail "failed to init GLFW")
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 2)
    GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True)
    GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
    GLFW.createWindow (x size) (y size) title Nothing Nothing >>= \case
        Nothing  -> do
            GLFW.terminate
            fail "failed to create window"
        Just win -> do
            GLFW.makeContextCurrent (Just win)
            GLFW.setWindowPos win (x origin) (y origin)
            return win

destroyWindow :: GLFW.Window -> IO ()
destroyWindow win = do
    GLFW.destroyWindow win
    GLFW.terminate

errorCB :: GLFW.ErrorCallback
errorCB _ desc = hPutStrLn stderr desc

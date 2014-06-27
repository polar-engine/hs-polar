module Polar.Engine where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
import System.IO (stdout, stderr, hPutStrLn, hSetBuffering, BufferMode(..))
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Polar.Types.Engine
import Polar.Types.Rectangle
import Polar.Types.Point2
import Polar.Types.Color

run :: StateT Engine IO ()
run = do
    size' <- gets (size . viewport)
    win <- gets title >>= liftIO . setupGLFW size'
    setup >> loop win >> shutdown >> liftIO (shutdownGLFW win)

setupGLFW :: Point2 Int -> String -> IO GLFW.Window
setupGLFW (Point2 width height) title = do
    GLFW.setErrorCallback (Just errorCB)
    GLFW.init >>= flip unless (fail "GLFW.init")
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 2)
    GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True)
    GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
    GLFW.windowHint (GLFW.WindowHint'ContextRobustness GLFW.ContextRobustness'NoResetNotification)
    GLFW.createWindow width height title Nothing Nothing
        >>= maybe (GLFW.terminate >> fail "GLFW.createWindow") setupWindow

setupWindow :: GLFW.Window -> IO GLFW.Window
setupWindow win = do
    GLFW.makeContextCurrent (Just win)
    GLFW.setKeyCallback win Nothing
    return win

setup :: StateT Engine IO ()
setup = do
    liftIO $ do
        hSetBuffering stdout NoBuffering
        GLFW.swapInterval 1
        GL.clearColor $= colorToGL navyBlueColor

loop :: GLFW.Window -> StateT Engine IO ()
loop win = liftIO (GLFW.windowShouldClose win) >>= \close -> unless close $ do
    liftIO $ do
        GL.clear [GL.ColorBuffer]
        GLFW.swapBuffers win
        GLFW.pollEvents
    loop win

shutdown :: StateT Engine IO ()
shutdown = return ()

shutdownGLFW :: GLFW.Window -> IO ()
shutdownGLFW win = GLFW.destroyWindow win >> GLFW.terminate

errorCB :: GLFW.ErrorCallback
errorCB _ desc = hPutStrLn stderr desc

--keyCB :: GLFW.KeyCallback
--keyCB win key scancode action mods = return ()

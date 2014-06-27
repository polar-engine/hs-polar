module Polar.Engine where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
import System.IO (stdout, hSetBuffering, BufferMode(..))
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Polar.Types.Engine
import Polar.Types.Rectangle
import Polar.Types.Point2
import Polar.Types.Color

run :: StateT Engine IO GLFW.Window
run = do
    get >>= liftIO . print
    size' <- gets (size . viewport)
    win <- gets title >>= liftIO . setupGLFW size'
    setup win
    return win

setupGLFW :: Point2 Int -> String -> IO GLFW.Window
setupGLFW (Point2 width height) title = do
    GLFW.init >>= flip unless (fail "GLFW.init")
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 2)
    GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True)
    GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
    GLFW.createWindow width height title Nothing Nothing
        >>= maybe (GLFW.terminate >> fail "GLFW.createWindow") setupWindow
  where setupWindow win = GLFW.makeContextCurrent (Just win) >> return win

setup :: GLFW.Window -> StateT Engine IO ()
setup _ = do
    liftIO $ do
        hSetBuffering stdout NoBuffering
        GLFW.swapInterval 1
        GL.clearColor $= colorToGL navyBlueColor

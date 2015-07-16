module Polar.Engine where

import Data.IORef
import qualified Data.Foldable as Foldable (mapM_)
import qualified Data.Sequence as Seq
import Control.Monad (when, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
import System.IO (stdout, stderr, hPutStrLn, hSetBuffering, BufferMode(..))
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Polar.Types
import Polar.Hoist
import Polar.Input

run :: StateT Engine IO ()
run = do
    size' <- gets (size . engineViewport)
    win <- gets engineTitle >>= liftIO . setupGLFW size'
    eventQueueRef <- liftIO (newIORef Seq.empty)
    --let keyCB = modifyIORef eventQueueRef . flip (Seq.|>) . fromGLFWKeyCB
    let keyCB _ key _ act mods = modifyIORef eventQueueRef
            $ flip (Seq.|>) $ KeyEvent (fromGLFWKey key) KeyDownAction (KeyModifiers False False False False)
    liftIO (GLFW.setKeyCallback win (Just keyCB))
    setup >> loop win eventQueueRef >> shutdown >> liftIO (shutdownGLFW win)

setupGLFW :: Point Int -> String -> IO GLFW.Window
setupGLFW (Point3 width height _) title = setupGLFW (Point2 width height) title
setupGLFW (Point2 width height) title = do
    GLFW.setErrorCallback (Just errorCB)
    GLFW.init >>= flip unless (fail "GLFW.init")
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 2)
    GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True)
    GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
    GLFW.createWindow width height title Nothing Nothing
        >>= maybe (GLFW.terminate >> fail "GLFW.createWindow") setupWindow
  where setupWindow win = GLFW.makeContextCurrent (Just win) >> return win

setup :: StateT Engine IO ()
setup = do
    liftIO $ do
        hSetBuffering stdout NoBuffering
        GLFW.swapInterval 1
        GL.clearColor $= colorToGL navyBlueColor

loop :: GLFW.Window -> IORef (Seq.Seq Event) -> StateT Engine IO ()
loop win eventQueueRef = liftIO (GLFW.windowShouldClose win) >>= \close -> unless close $ do
    liftIO $ do
        GL.clear [GL.ColorBuffer]
        GLFW.swapBuffers win
        GLFW.pollEvents
        esc <- GLFW.getKey win GLFW.Key'Escape
        when (esc == GLFW.KeyState'Pressed) $ GLFW.setWindowShouldClose win True
    liftIO (readIORef eventQueueRef) >>= hoistState . Foldable.mapM_ handleEvent
    loop win eventQueueRef

handleEvent :: Event -> State Engine ()
handleEvent e = return ()

shutdown :: StateT Engine IO ()
shutdown = return ()

shutdownGLFW :: GLFW.Window -> IO ()
shutdownGLFW win = GLFW.destroyWindow win >> GLFW.terminate

errorCB :: GLFW.ErrorCallback
errorCB _ desc = hPutStrLn stderr desc

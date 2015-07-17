{-# LANGUAGE LambdaCase #-}

module Polar.Engine where

import Data.IORef
import qualified Data.Foldable as Foldable (mapM_)
import qualified Data.Sequence as Seq
import Control.Monad.State
import System.IO (stdout, stderr, hPutStrLn, hSetBuffering, BufferMode(..))
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Polar.Types
import Polar.Hoist
import Polar.Input

run :: StateT Engine IO ()
run = do
    viewport <- gets engineViewport
    title <- gets engineTitle
    win <- liftIO (setupGLFW viewport title)
    eventQueueRef <- liftIO (newIORef Seq.empty)
    --let keyCB = modifyIORef eventQueueRef . flip (Seq.|>) . fromGLFWKeyCB
    let keyCB _ key _ act mods = modifyIORef eventQueueRef
            $ flip (Seq.|>) $ KeyEvent (fromGLFWKey key) KeyDownAction (KeyModifiers False False False False)
    liftIO (GLFW.setKeyCallback win (Just keyCB))
    setup
    loop win eventQueueRef
    shutdown
    liftIO (shutdownGLFW win)

setupGLFW :: Box Int -> String -> IO GLFW.Window
setupGLFW (Box origin size) title = do
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

setup :: StateT Engine IO ()
setup = do
    liftIO $ do
        hSetBuffering stdout NoBuffering
        GLFW.swapInterval 1
        GL.clearColor $= colorToGL navyBlueColor

loop :: GLFW.Window -> IORef (Seq.Seq Event) -> StateT Engine IO ()
loop win eventQueueRef = liftIO (GLFW.windowShouldClose win) >>= \close -> unless close $ do
    liftIO $ do
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
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
shutdownGLFW win = do
    GLFW.destroyWindow win
    GLFW.terminate

errorCB :: GLFW.ErrorCallback
errorCB _ desc = hPutStrLn stderr desc

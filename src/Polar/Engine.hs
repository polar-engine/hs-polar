{-# LANGUAGE LambdaCase #-}

module Polar.Engine where

import Data.Function.Apply
import Control.Monad.State
import Control.Lens (use)
import System.IO (hSetBuffering, stdout, stderr, BufferMode(..))
import Polar.Types
import Polar.Listener (notify)

run :: PolarIO ()
run = do
    liftIO $ do
        hSetBuffering stdout NoBuffering
        hSetBuffering stderr LineBuffering
    use startup >>= id
    notify StartupEvent StartupNote
    loop
    notify ShutdownEvent ShutdownNote

loop :: PolarIO ()
loop = use willExit >>= flip unless `apply` do
    notify TickEvent (TickNote 1)
    loop

{-
run' :: PolarIO ()
run' = do
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

setup :: PolarIO ()
setup = do
    liftIO $ do
        hSetBuffering stdout NoBuffering
        GLFW.swapInterval 1
        GL.clearColor $= colorToGL navyBlueColor

loop :: GLFW.Window -> IORef (Seq.Seq Event) -> PolarIO ()
loop win eventQueueRef = liftIO (GLFW.windowShouldClose win) >>= \close -> unless close $ do
    liftIO $ do
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        GLFW.swapBuffers win
        GLFW.pollEvents
        esc <- GLFW.getKey win GLFW.Key'Escape
        when (esc == GLFW.KeyState'Pressed) $ GLFW.setWindowShouldClose win True
    liftIO (readIORef eventQueueRef) >>= hoistState . Foldable.mapM_ handleEvent
    loop win eventQueueRef

handleEvent :: Event -> Polar ()
handleEvent e = return ()
-}

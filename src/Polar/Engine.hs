{-# LANGUAGE LambdaCase #-}

module Polar.Engine where

import Control.Monad.State
import Polar.Types
import Polar.Listener (notify)

run :: PolarIO ()
run = do
    gets engineStartup >>= id
    notify StartupNote
    notify ShutdownNote

{-
startup :: PolarIO ()
startup = gets engineSystems >>= mapM_ startupOne
  where startupOne sys = sysStartup sys >>= \case
            Nothing  -> liftIO $ hPutStrLn stderr ('[' : sysName sys ++ "] started up")
            Just err -> liftIO $ hPutStrLn stderr ('[' : sysName sys ++ "] failed to start up (" ++ err ++ ")")

shutdown :: PolarIO ()
shutdown = gets (engineSystems) >>= mapM_ shutdownOne . reverse
  where shutdownOne sys = sysShutdown sys >>= \case
            Nothing  -> liftIO $ hPutStrLn stderr ('[' : sysName sys ++ "] shut down")
            Just err -> liftIO $ hPutStrLn stderr ('[' : sysName sys ++ "] failed to shut down (" ++ err ++ ")")
-}
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

shutdown' :: PolarIO ()
shutdown' = return ()
-}

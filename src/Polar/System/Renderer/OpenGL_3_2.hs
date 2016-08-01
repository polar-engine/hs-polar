{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE LambdaCase #-}

{-|
  Module      : Polar.System.Renderer.OpenGL_3_2
  Copyright   : (c) 2016 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : non-portable (GHC extensions)

  OpenGL 3.2 renderer.
-}

module Polar.System.Renderer.OpenGL_3_2 (renderer) where

import Data.Foldable (traverse_)
import Control.Monad.RWS (MonadIO, liftIO, tell)
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Polar.Types
import Polar.Log
import Polar.Storage

renderer :: System
renderer = defaultSystem "OpenGL 3.2 Renderer"
    & startup  .~ tell [SysCoreAction startupF]
    & tick     .~ tell [SysCoreAction tickF]
    & shutdown .~ tell [SysCoreAction shutdownF]

startupF :: Core ()
startupF = do
    liftIO $ do
        GLFW.init
        GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
        GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 2)
        GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True)
        GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
    liftIO (GLFW.createWindow 1280 720 "Polar Game Engine" Nothing Nothing) >>= \case
        Nothing  -> logFatal "Failed to create window"
        Just win -> do
            logWrite DEBUG "Created window"
            liftIO $ GLFW.makeContextCurrent (Just win)
            store win "window"
            gl (GL.clearColor $= GL.Color4 0 0 0 0)

tickF :: Core ()
tickF = do
    gl (GL.clear [GL.ColorBuffer, GL.DepthBuffer])
    liftIO . GLFW.swapBuffers =<< forceRetrieve as "window"
    liftIO GLFW.pollEvents

shutdownF :: Core ()
shutdownF = do
    liftIO . GLFW.destroyWindow =<< forceRetrieve as "window"
    logWrite DEBUG "Destroyed window"
    liftIO GLFW.terminate

gl :: (MonadIO m, LogPolar m) => IO a -> m a
gl io = do
    result <- liftIO io
    traverse_ logGLError =<< liftIO (GL.get GL.errors)
    pure result
  where logGLError (GL.Error category msg) = logWrite CRITICAL $
            "OpenGL error (Category = " ++ show category ++ ", " ++ msg ++ ")"

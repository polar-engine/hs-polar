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

import Control.Monad.RWS (liftIO, tell)
import qualified Graphics.UI.GLFW as GLFW
import Polar.Types
import Polar.Log
import Polar.Storage

renderer :: System
renderer = defaultSystem "OpenGL 3.2 Renderer"
    & startup  .~ startupF
    & shutdown .~ shutdownF

startupF :: Sys ()
startupF = tell [SysCoreAction startupWindow]

startupWindow :: Core ()
startupWindow = do
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

shutdownF :: Sys ()
shutdownF = tell [SysCoreAction shutdownWindow]

shutdownWindow :: Core ()
shutdownWindow = do
    liftIO . GLFW.destroyWindow =<< forceRetrieve as "window"
    logWrite DEBUG "Destroyed window"
    liftIO GLFW.terminate

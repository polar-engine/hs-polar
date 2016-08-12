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

import Data.Bool (bool)
import Data.Foldable (traverse_)
import Control.Monad.RWS (MonadIO, liftIO, tell)
import Foreign (nullPtr, sizeOf, withArray)
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Polar.Types
import Polar.Log
import Polar.Exit
import Polar.Storage

renderer :: System
renderer = defaultSystem "OpenGL 3.2 Renderer"
    & startup  .~ tell [SysCoreAction startupF]
    & tick     .~ tell [SysCoreAction tickF]
    & shutdown .~ tell [SysCoreAction shutdownF]

vertices :: [GL.GLfloat]
vertices = [ -1, -1
           ,  1, -1
           ,  0,  1
           ]

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
            storeNamed win "window"
            liftIO $ GLFW.makeContextCurrent (Just win)
            gl (GL.clearColor $= GL.Color4 0.02 0.05 0.1 0)
            setupVAO

setupVAO :: Core ()
setupVAO = do
    vao <- gl GL.genObjectName
    gl (GL.bindVertexArrayObject $= Just vao)
    vbo <- gl GL.genObjectName
    gl (GL.bindBuffer GL.ArrayBuffer $= Just vbo)
    gl $ withArray vertices $ \buffer -> do
        let len = length vertices * sizeOf (head vertices)
        GL.bufferData GL.ArrayBuffer $= (fromIntegral len, buffer, GL.StaticDraw)
    store vao
    gl $ GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 nullPtr)
    gl $ GL.vertexAttribArray   (GL.AttribLocation 0) $= GL.Enabled

tickF :: Core ()
tickF = do
    win <- retrieveNamed "window"
    bool (render win) exit =<< liftIO (GLFW.windowShouldClose win)

render :: GLFW.Window -> Core ()
render win = do
    gl (GL.clear [GL.ColorBuffer, GL.DepthBuffer])
    traverse_ renderOne =<< retrieveAll
    liftIO (GLFW.swapBuffers win)
    liftIO GLFW.pollEvents

renderOne :: GL.VertexArrayObject -> Core ()
renderOne vao = do
    gl (GL.bindVertexArrayObject $= Just vao)
    gl (GL.drawArrays GL.Triangles 0 (fromIntegral $ length vertices))

shutdownF :: Core ()
shutdownF = do
    liftIO . GLFW.destroyWindow =<< retrieveNamed "window"
    logWrite DEBUG "Destroyed window"
    liftIO GLFW.terminate

gl :: (MonadIO m, LogPolar m) => IO a -> m a
gl io = do
    result <- liftIO io
    traverse_ logGLError =<< liftIO (GL.get GL.errors)
    pure result
  where logGLError (GL.Error category msg) = logWrite CRITICAL $
            "OpenGL error (Category = " ++ show category ++ ", " ++ msg ++ ")"

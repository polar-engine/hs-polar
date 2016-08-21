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
import Data.Hashable (Hashable)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import Control.Monad.RWS (MonadIO, void, liftIO, tell)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan
import Foreign (nullPtr, sizeOf, withArray)
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GL.Core32 as GL
import qualified Graphics.UI.GLFW as GLFW
import Polar.Types
import Polar.Log
import Polar.Exit
import Polar.Storage
import Polar.System.Renderer
import Polar.Shader (compile)
import Polar.Shader.Types
import Polar.Shader.Compiler.GLSL150 (GLSL150(..))

instance Hashable GLFW.Key
type Drawable = (GL.VertexArrayObject, Int)

projection :: Floating a => a -> a -> a -> [a]
projection fov zNear zFar = [ s, 0, 0,                    0
                            , 0, s, 0,                    0
                            , 0, 0, negFarLimit,         -1
                            , 0, 0, negFarLimit * zNear,  1
                            ]
  where s = recip $ tan (fov * 0.5 * pi / 180.0)
        negFarLimit = zFar / (zFar - zNear)

translation :: Floating a => a -> a -> a -> [a]
translation x y z = [ 1, 0, 0, 0
                    , 0, 1, 0, 0
                    , 0, 0, 1, 0
                    , x, y, z, 1
                    ]

xRotation :: Floating a => a -> [a]
xRotation x = [ 1,            0,         0, 0
              , 0,   cos theta , sin theta, 0
              , 0, -(sin theta), cos theta, 0
              , 0,            0,         0, 1
              ]
  where theta = -x

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
            storeKeyed "window" win
            chan <- use msgQueue
            liftIO $ GLFW.setKeyCallback win (Just (keyCB chan))
            liftIO $ GLFW.makeContextCurrent (Just win)
            program <- createProgram "main.shader"
            storeKeyed "program" program
            gl (GL.currentProgram $= Just program)
            GL.UniformLocation loc <- gl (GL.uniformLocation program "u_projection")
            gl $ withArray (projection 70 1 1000) $ \buffer ->
                GL.glUniformMatrix4fv loc 1 0 buffer
            gl (GL.clearColor $= GL.Color4 0.02 0.05 0.1 0)

tickF :: Core ()
tickF = do
    win <- retrieveKeyed "window"
    bool (render win) exit =<< liftIO (GLFW.windowShouldClose win)
    mRetrieveKeyed GLFW.Key'Escape >>= \case
        Just True -> exit
        _         -> pure ()
    readRendererMsg >>= \case
        Just (_, prim) -> void $ store =<< createDrawable prim
        Nothing        -> pure ()
    translate
    rotate

translate :: Core ()
translate = do
    program <- retrieveKeyed "program"
    GL.UniformLocation loc <- gl (GL.uniformLocation program "u_translation")
    gl $ withArray (translation 0 (-0.2) (-5)) $ \buffer ->
        GL.glUniformMatrix4fv loc 1 0 buffer

rotate :: Core ()
rotate = do
    x <- maybe 0 (+ 0.05) <$> mRetrieveKeyedP "xrot" (Proxy :: Proxy Float)
    storeKeyed "xrot" x
    program <- retrieveKeyed "program"
    GL.UniformLocation loc <- gl (GL.uniformLocation program "u_rotation")
    gl $ withArray (xRotation x) $ \buffer ->
        GL.glUniformMatrix4fv loc 1 0 buffer

render :: GLFW.Window -> Core ()
render win = do
    gl (GL.clear [GL.ColorBuffer, GL.DepthBuffer])
    traverse_ renderOne =<< retrieveAll
    liftIO (GLFW.swapBuffers win)
    liftIO GLFW.pollEvents

renderOne :: Drawable -> Core ()
renderOne (vao, n) = do
    gl (GL.bindVertexArrayObject $= Just vao)
    gl (GL.drawArrays GL.Triangles 0 (fromIntegral n))

shutdownF :: Core ()
shutdownF = do
    liftIO . GLFW.destroyWindow =<< retrieveKeyed "window"
    logWrite DEBUG "Destroyed window"
    liftIO GLFW.terminate

createProgram :: String -> Core GL.Program
createProgram path = f <$> liftIO (readFile path) >>= \case
    Right (vertex, pixel) -> do
        vsh <- createGLShader vertex GL.VertexShader
        fsh <- createGLShader pixel GL.FragmentShader
        createGLProgram [vsh, fsh]
    Left err -> logFatal ("Failed to create program: " ++ err)
  where f contents = compile contents
            (M.fromList [ ("projection",  DataMatrix4x4)
                        , ("translation", DataMatrix4x4)
                        , ("rotation",    DataMatrix4x4)
                        ])
            (M.fromList [("vertex", DataFloat3)])
            (M.fromList [("color", DataFloat4)]) GLSL150

createGLShader :: String -> GL.ShaderType -> Core GL.Shader
createGLShader src ty = do
    shader <- gl (GL.createShader ty)
    gl (GL.shaderSourceBS shader $= BS.pack src)
    gl (GL.compileShader shader)
    gl (GL.get $ GL.compileStatus shader) >>= \case
        True  -> pure shader
        False -> do
            log <- gl (GL.get $ GL.shaderInfoLog shader)
            logFatal ("Failed to create shader:\n" ++ log)

createGLProgram :: [GL.Shader] -> Core GL.Program
createGLProgram shaders = do
    program <- gl GL.createProgram
    gl (GL.attachedShaders program $= shaders)
    gl (GL.linkProgram program)
    gl (GL.get $ GL.linkStatus program) >>= \case
        True  -> pure program
        False -> do
            log <- gl (GL.get $ GL.programInfoLog program)
            logFatal ("Failed to create program:\n" ++ log)

createDrawable :: Primitive -> Core Drawable
createDrawable vertices = do
    vao <- gl GL.genObjectName
    gl (GL.bindVertexArrayObject $= Just vao)
    vbo <- gl GL.genObjectName
    gl (GL.bindBuffer GL.ArrayBuffer $= Just vbo)
    gl $ withArray vertices $ \buffer -> do
        let len = length vertices * sizeOf (head vertices)
        GL.bufferData GL.ArrayBuffer $= (fromIntegral len, buffer, GL.StaticDraw)
    gl $ GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 nullPtr)
    gl $ GL.vertexAttribArray   (GL.AttribLocation 0) $= GL.Enabled
    pure (vao, length vertices)

keyCB :: TChan CoreMsg -> GLFW.KeyCallback
keyCB chan _ key _ act _ = liftIO $ atomically (writeTChan chan msg)
  where msg = storeKeyedMsg key True

gl :: (MonadIO m, LogPolar m) => IO a -> m a
gl io = do
    result <- liftIO io
    traverse_ logGLError =<< liftIO (GL.get GL.errors)
    pure result
  where logGLError (GL.Error category msg) = logWrite CRITICAL $
            "OpenGL error (Category = " ++ show category ++ ", " ++ msg ++ ")"

{-# LANGUAGE LambdaCase #-}

module Polar.Renderer.OpenGL_3_2 where

import Data.List (intercalate)
import Data.Function.Apply
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import Control.Monad.State (unless, liftIO)
import Control.Lens ((^.), _1)
import System.IO (stderr, hPutStrLn)
import Foreign (nullPtr, sizeOf, withArray)
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Graphics.UI.GLFW as GLFW
import Polar.Types
import Polar.Control
import Polar.Listener
import Polar.Shader (compile)
import Polar.Shader.Types (DataType(..))
import Polar.Shader.Compiler.GLSL150 (GLSL150(..))

type Drawable = (Int, GL.VertexArrayObject, GL.BufferObject)

projection :: [GL.GLfloat]
projection = [ 1.42815, 0.0,     0.0,       0.0
             , 0.0,     1.42815, 0.0,       0.0
             , 0.0,     0.0,     1.001001, -1.0
             , 0.0,     0.0,     1.001001,  1.0
             ]

startup :: ListenerF ()
startup _ _ = setupWindow viewport title >>= \case
    Nothing  -> return ()
    Just win -> setupShader >>= \case
        Nothing      -> return ()
        Just program -> do
            (GL.UniformLocation loc) <- gl (GL.uniformLocation program "u_projection")
            gl $ withArray projection $ \buffer -> GL.glUniformMatrix4fv loc 1 0 buffer
            drawable <- setupDrawable [ -1, -1
                                      ,  1, -1
                                      ,  0,  1
                                      ]
            gl $ GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
            gl (GL.clearColor $= GL.Color4 0 0 0 0)
            listen "tick" (Listener (render win drawable))
            listen "shutdown" (Listener (shutdown win))
  where viewport = Box (Point 50 50 0 0) (Point 640 360 0 0)
        title = "Game"

render :: GLFW.Window -> Drawable -> ListenerF ()
render win drawable _ _ = liftIO (GLFW.windowShouldClose win) >>= \case
    True  -> exit
    False -> do
        gl $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        gl $ GL.drawArrays GL.Triangles 0 (fromIntegral (drawable ^. _1))
        liftIO (GLFW.swapBuffers win)
        liftIO GLFW.pollEvents

shutdown :: GLFW.Window -> ListenerF ()
shutdown win _ _ = liftIO (destroyWindow win)

gl :: IO a -> PolarIO a
gl action = do
    result <- liftIO action
    liftIO (GL.get GL.errors) >>= unlessEmpty `apply` \xs -> notify "error" (intercalate "\n" (showGLError <$> xs))
    return result
  where showGLError (GL.Error category message) = show category ++ " (" ++ message ++ ")"
        unlessEmpty f xs = unless (null xs) (f xs)

setupDrawable :: [GL.GLfloat] -> PolarIO Drawable
setupDrawable vertices = do
    vao <- gl GL.genObjectName
    gl $ GL.bindVertexArrayObject $= Just vao
    vbo <- gl GL.genObjectName
    gl $ GL.bindBuffer GL.ArrayBuffer $= Just vbo
    gl $ withArray vertices $ \buffer ->
        GL.bufferData GL.ArrayBuffer $= (fromIntegral (length vertices * sizeOf (head vertices)), buffer, GL.StaticDraw)
    gl $ GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 nullPtr)
    return (length vertices, vao, vbo)

makeShader :: String -> GL.ShaderType -> PolarIO GL.Shader
makeShader contents shaderType = do
    shader <- gl (GL.createShader shaderType)
    gl (GL.shaderSourceBS shader $= BS.pack contents)
    gl (GL.compileShader shader)
    status <- gl $ GL.get (GL.compileStatus shader)
    unless status $ do
        infoLog <- gl $ GL.get (GL.shaderInfoLog shader)
        notify "error" infoLog
    return shader

makeProgram :: [GL.Shader] -> PolarIO GL.Program
makeProgram shaders = do
    program <- gl GL.createProgram
    gl (GL.attachedShaders program $= shaders)
    gl (GL.linkProgram program)
    status <- gl $ GL.get (GL.linkStatus program)
    unless status $ do
        infoLog <- gl $ GL.get (GL.programInfoLog program)
        notify "error" infoLog
    return program

setupShader :: PolarIO (Maybe GL.Program)
setupShader = f <$> liftIO (readFile "main.shader") >>= \case
    Left err              -> notify "error" err >> return Nothing
    Right (vertex, pixel) -> do
        vsh <- makeShader vertex GL.VertexShader
        fsh <- makeShader pixel GL.FragmentShader
        program <- makeProgram [vsh, fsh]
        gl (GL.currentProgram $= Just program)
        return (Just program)
  where f contents = compile contents
            (M.fromList [("projection", DataMatrix4x4)])
            (M.fromList [("vertex", DataFloat2)])
            (M.fromList [("color", DataFloat4)]) GLSL150

setupWindow :: Box Int -> String -> PolarIO (Maybe GLFW.Window)
setupWindow (Box origin size) title = do
    liftIO $ GLFW.setErrorCallback (Just errorCB)
    liftIO GLFW.init >>= \succeeded -> unless succeeded `apply` notify "error" "failed to init GLFW"
    liftIO $ do
        GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
        GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 2)
        GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True)
        GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
    liftIO `apply` GLFW.createWindow (size ^. x) (size ^. y) title Nothing Nothing >>= \case
        Nothing  -> do
            liftIO GLFW.terminate
            notify "error" "failed to create window"
            return Nothing
        Just win -> do
            liftIO $ GLFW.makeContextCurrent (Just win)
            liftIO $ GLFW.setWindowPos win (origin ^. x) (origin ^. y)
            return (Just win)

destroyWindow :: GLFW.Window -> IO ()
destroyWindow win = do
    GLFW.destroyWindow win
    GLFW.terminate

-- TODO: communicate errors back into PolarIO with a channel
errorCB :: GLFW.ErrorCallback
errorCB _ desc = hPutStrLn stderr desc

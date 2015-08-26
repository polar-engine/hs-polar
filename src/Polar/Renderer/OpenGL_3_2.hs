{-# LANGUAGE LambdaCase #-}

module Polar.Renderer.OpenGL_3_2 where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import Control.Applicative ((<$>))
import Control.Monad (unless)
import Control.Monad.State (liftIO)
import System.IO (stderr, hPutStrLn)
import Foreign (nullPtr)
import Foreign.Storable (sizeOf)
import Foreign.Marshal.Array (withArray)
import GHC.Stack (currentCallStack, renderStack)
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Polar.Types
import Polar.Control
import Polar.Listener
import Polar.Shader (compile)
import Polar.Shader.Compiler.GLSL150 (GLSL150(..))

vertices :: [GL.GLfloat]
vertices = [ -1, -1
           ,  1, -1
           ,  0,  1
           ]

startup :: Listener
startup _ = do
    win <- liftIO (setupWindow viewport title)
    liftIO setupVertices
    liftIO setupShader
    liftIO . gl $ GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
    liftIO (GL.clearColor $= GL.Color4 0 0 0 0)
    listen TickEvent (render win)
    listen ShutdownEvent (shutdown win)
  where viewport = Box (Point 50) (Point2 640 360)
        title = "Game"

render :: GLFW.Window -> Listener
render win _ = liftIO (GLFW.windowShouldClose win) >>= \case
    True  -> exit
    False -> liftIO $ do
        gl $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        gl $ GL.drawArrays GL.Triangles 0 (fromIntegral (length vertices))
        GLFW.swapBuffers win
        GLFW.pollEvents

shutdown :: GLFW.Window -> Listener
shutdown win _ = liftIO (destroyWindow win)

gl :: IO a -> IO a
gl action = do
    result <- action
    GL.get GL.errors >>= unlessEmpty (\xs -> do
            mapM_ printError xs
            currentCallStack >>= unlessEmpty (putStrLn . renderStack . init . init)
        )
    return result
  where printError (GL.Error category message) = putStrLn ("[ERROR] " ++ show category ++ " (" ++ message ++ ")")
        unlessEmpty f xs = unless (null xs) (f xs)

setupVertices :: IO (GL.VertexArrayObject, GL.BufferObject)
setupVertices = do
    vao <- gl GL.genObjectName
    gl $ GL.bindVertexArrayObject $= Just vao
    vbo <- gl GL.genObjectName
    gl $ GL.bindBuffer GL.ArrayBuffer $= Just vbo
    withArray vertices $ \buffer -> gl $
        GL.bufferData GL.ArrayBuffer $= (fromIntegral (length vertices * sizeOf (head vertices)), buffer, GL.StaticDraw)
    gl $ GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 nullPtr)
    return (vao, vbo)

makeShader :: String -> GL.ShaderType -> IO GL.Shader
makeShader contents shaderType= do
    shader <- gl (GL.createShader shaderType)
    gl (GL.shaderSourceBS shader $= BS.pack contents)
    gl (GL.compileShader shader)
    status <- gl $ GL.get (GL.compileStatus shader)
    unless status $ do
        infoLog <- gl $ GL.get (GL.shaderInfoLog shader)
        putStrLn ("[INFOLOG] " ++ infoLog)
    return shader

makeProgram :: [GL.Shader] -> IO GL.Program
makeProgram shaders = do
    program <- gl GL.createProgram
    gl (GL.attachedShaders program $= shaders)
    gl (GL.linkProgram program)
    status <- gl $ GL.get (GL.linkStatus program)
    unless status $ do
        infoLog <- gl $ GL.get (GL.programInfoLog program)
        putStrLn ("[INFOLOG] " ++ infoLog)
    return program

setupShader :: IO ()
setupShader = f <$> readFile "main.shader" >>= \case
    Left err              -> putStrLn err
    Right (vertex, pixel) -> do
        vsh <- makeShader vertex GL.VertexShader
        fsh <- makeShader pixel GL.FragmentShader
        program <- makeProgram [vsh, fsh]
        gl (GL.currentProgram $= Just program)
  where f contents = compile contents (M.fromList [("vertex", 2)]) (M.fromList [("color", 4)]) GLSL150

setupWindow :: Box Int -> String -> IO GLFW.Window
setupWindow (Box origin size) title = do
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

destroyWindow :: GLFW.Window -> IO ()
destroyWindow win = do
    GLFW.destroyWindow win
    GLFW.terminate

errorCB :: GLFW.ErrorCallback
errorCB _ desc = hPutStrLn stderr desc

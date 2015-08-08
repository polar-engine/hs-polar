{-# LANGUAGE LambdaCase #-}

module Polar.Renderer.OpenGL_3_2 where

import qualified Data.ByteString as BS
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

vertices :: [GL.GLfloat]
vertices = [ -1, -1
           ,  1, -1
           ,  0,  1
           ]

gl :: IO a -> IO a
gl action = do
    result <- action
    GL.get GL.errors >>= \case
        [] -> return ()
        xs -> do
            mapM_ printError xs
            currentCallStack >>= \case
                [] -> return ()
                stack -> putStrLn $ (renderStack . init . init) stack
    return result
  where printError (GL.Error category message) = putStrLn ("[ERROR] " ++ show category ++ " (" ++ message ++ ")")

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

setupShaders :: IO ()
setupShaders = do
    vsh <- gl $ GL.createShader GL.VertexShader
    BS.readFile "shader.gls" >>= \s -> gl $ GL.shaderSourceBS vsh $= s
    --gl $ GL.compileShader vsh

    status <- gl $ GL.get (GL.compileStatus vsh)
    unless status $ do
        infoLog <- gl $ GL.get (GL.shaderInfoLog vsh)
        putStrLn ("[INFOLOG] " ++ infoLog)

    fsh <- gl $ GL.createShader GL.FragmentShader
    BS.readFile "shader.fsh" >>= \s -> gl $ GL.shaderSourceBS fsh $= s
    gl $ GL.compileShader fsh

    status <- gl $ GL.get (GL.compileStatus fsh)
    unless status $ do
        infoLog <- gl $ GL.get (GL.shaderInfoLog fsh)
        putStrLn ("[INFOLOG] " ++ infoLog)

    program <- gl GL.createProgram
    gl $ GL.attachedShaders program $= [vsh, fsh]
    gl $ GL.linkProgram program

    status <- gl $ GL.get (GL.linkStatus program)
    unless status $ do
        infoLog <- gl $ GL.get (GL.programInfoLog program)
        putStrLn ("[INFOLOG] " ++ infoLog)

    gl $ GL.currentProgram $= Just program
    gl $ GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled

startup :: Listener
startup _ = do
    win <- liftIO $ setupWindow (Box (Point 50) (Point2 640 360)) "Game"
    objs <- liftIO setupVertices
    liftIO setupShaders
    liftIO (GL.clearColor $= GL.Color4 0 0 0 0)
    listen TickEvent (render win objs)
    listen ShutdownEvent (shutdown win)

render :: GLFW.Window -> (GL.VertexArrayObject, GL.BufferObject) -> Notification -> PolarIO ()
render win (vao, vbo) _ = liftIO (GLFW.windowShouldClose win) >>= \case
    True  -> exit
    False -> liftIO $ do
        gl $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        gl $ GL.drawArrays GL.Triangles 0 (fromIntegral (length vertices))
        GLFW.swapBuffers win
        GLFW.pollEvents

shutdown :: GLFW.Window -> Listener
shutdown win _ = liftIO (destroyWindow win)

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

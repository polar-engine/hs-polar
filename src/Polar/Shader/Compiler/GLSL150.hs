{-# LANGUAGE LambdaCase #-}

module Polar.Shader.Compiler.GLSL150 where

import Data.Maybe (fromMaybe)
import Data.List (intersperse)
import qualified Data.Map as M
import Control.Monad.RWS (RWST, asks, tell, get, put, lift, runRWST)
import Polar.Shader.Types

data GLSL150 = GLSL150
instance Compiler GLSL150 where generate env _ = (\(_, _, w) -> w) <$> runRWST writeShaders env ShaderVertex

type ShaderM = RWST CompilerEnv (String, String) ShaderType (Either String)

unrecognized :: String -> ShaderM a
unrecognized name = lift $ Left ("unrecognized name (" ++ name ++ ")")

tellCurrent :: String -> ShaderM ()
tellCurrent msg = get >>= \case
    ShaderVertex -> tell (msg, "")
    ShaderPixel  -> tell ("", msg)

showType :: DataType -> String
showType DataFloat     = "float"
showType DataFloat2    = "vec2"
showType DataFloat4    = "vec4"
showType DataMatrix4x4 = "mat4x4"

writeAST :: AST -> ShaderM ()
writeAST (Assignment lhs rhs) = do
    tellCurrent "("
    writeAST lhs
    tellCurrent "="
    writeAST rhs
    tellCurrent ")"
writeAST (Additive lhs rhs) = do
    tellCurrent "("
    writeAST lhs
    tellCurrent "+"
    writeAST rhs
    tellCurrent ")"
writeAST (Multiplicative lhs rhs) = do
    tellCurrent "("
    writeAST lhs
    tellCurrent "*"
    writeAST rhs
    tellCurrent ")"
writeAST ast@(Swizzle asts) = do
    ty <- lift (astType ast)
    tellCurrent ("(" ++ showType ty ++ "(")
    sequence (tellCurrent "," `intersperse` map writeAST asts)
    tellCurrent "))"
writeAST (Literal literal) = tellCurrent (show literal)
writeAST (Identifier name) = lift $ Left ("unresolved identifier (" ++ name ++ ")")
writeAST NamePosition = tellCurrent "gl_Position"
writeAST (NameGlobal name _) = tellCurrent ("u_" ++ name)
writeAST (NameInput name _) = tellCurrent ("a_" ++ name)
writeAST (NameOutput name _) = tellCurrent ("o_" ++ name)

writeFunction :: String -> Maybe String -> ShaderM ()
writeFunction name mActualName = M.lookup name <$> asks compilerFunctions >>= \case
    Nothing -> unrecognized name
    Just asts -> do
        tellCurrent ("void " ++ fromMaybe name mActualName ++ "(){")
        sequence (tellCurrent ";" `intersperse` map writeAST asts)
        tellCurrent ";}"

writeLocatables :: String -> String -> Int -> [(String, DataType)] -> ShaderM ()
writeLocatables _ _ _ [] = return ()
writeLocatables qualifier prefix n ((name, ty):xs) = do
    tellCurrent $ "layout(location=" ++ show n ++ ')' : qualifier ++ " " ++ showType ty ++ ' ' : prefix ++ name ++ ";"
    writeLocatables qualifier prefix (succ n) xs

writeShaders :: ShaderM ()
writeShaders = do
    put ShaderVertex
    tellCurrent (version ++ ext ++ precision)
    M.toList <$> asks compilerInputs >>= writeLocatables "in" "a_" 0
    writeFunction "vertex" (Just "main")
    put ShaderPixel
    tellCurrent (version ++ ext ++ precision)
    M.toList <$> asks compilerOutputs >>= writeLocatables "out" "o_" 0
    writeFunction "pixel" (Just "main")
  where version = "#version 150\n"
        ext = "#extension GL_ARB_explicit_attrib_location: enable\n"
        precision = "precision highp float;"

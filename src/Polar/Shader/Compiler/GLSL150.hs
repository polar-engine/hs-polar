{-# LANGUAGE LambdaCase #-}

module Polar.Shader.Compiler.GLSL150 where

import Data.Maybe (fromMaybe)
import Data.List (intersperse)
import qualified Data.Map as M
import Control.Applicative ((<$>), (<*>))
import Control.Monad.RWS (RWST, asks, tell, get, put, lift, runRWST)
import Polar.Shader.Types

data GLSL150 = GLSL150
instance Compiler GLSL150 where generate env _ = (\(_, _, w) -> w) <$> runRWST writeShaders env Vertex

type ShaderM = RWST CompilerEnv (String, String) Type (Either String)

unrecognized :: String -> ShaderM a
unrecognized name = lift $ Left ("unrecognized name (" ++ name ++ ")")

tellCurrent :: String -> ShaderM ()
tellCurrent msg = get >>= \case
    Vertex -> tell (msg, "")
    Pixel  -> tell ("", msg)

writeAST :: AST -> ShaderM ()
writeAST (Assignment lhs rhs) = do
    tellCurrent "("
    writeAST lhs
    tellCurrent "="
    writeAST rhs
    tellCurrent ")"
    lift $ (==) <$> astComponents lhs <*> astComponents rhs >>= \case
        False -> Left "number of components on lhs does not match number of components on rhs"
        True  -> return ()
writeAST ast@(Swizzle asts) = do
    components <- lift (astComponents ast)
    tellCurrent ("(vec" ++ show components ++ "(")
    sequence (tellCurrent "," `intersperse` map writeAST asts)
    tellCurrent "))"
writeAST (Literal literal) = tellCurrent (show literal)
writeAST (Identifier name) = lift $ Left ("unresolved identifier (" ++ name ++ ")")
writeAST NamePosition = tellCurrent "gl_Position"
writeAST (NameInput name _) = tellCurrent ("a_" ++ name)
writeAST (NameOutput name _) = tellCurrent ("o_" ++ name)

writeFunction :: String -> Maybe String -> ShaderM ()
writeFunction name mActualName = M.lookup name <$> asks compilerFunctions >>= \case
    Nothing -> unrecognized name
    Just asts -> do
        tellCurrent ("void " ++ fromMaybe name mActualName ++ "(){")
        sequence (tellCurrent ";" `intersperse` map writeAST asts)
        tellCurrent ";}"

writeLocatables :: String -> String -> Int -> [(String, Int)] -> ShaderM ()
writeLocatables _ _ _ [] = return ()
writeLocatables qualifier prefix n ((name, c):xs) = do
    tellCurrent $ "layout(location=" ++ show n ++ ')' : qualifier ++ " vec" ++ show c ++ ' ' : prefix ++ name ++ ";"
    writeLocatables qualifier prefix (succ n) xs

writeShaders :: ShaderM ()
writeShaders = do
    put Vertex
    tellCurrent (version ++ ext ++ precision)
    M.toList <$> asks compilerInputs >>= writeLocatables "in" "a_" 0
    writeFunction "vertex" (Just "main")
    put Pixel
    tellCurrent (version ++ ext ++ precision)
    M.toList <$> asks compilerOutputs >>= writeLocatables "out" "o_" 0
    writeFunction "pixel" (Just "main")
  where version = "#version 150\n"
        ext = "#extension GL_ARB_explicit_attrib_location: enable\n"
        precision = "precision highp float;"
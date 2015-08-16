{-# LANGUAGE LambdaCase #-}

module Polar.Renderer.OpenGL_3_2.Shader where

import Data.Maybe (fromMaybe)
import Data.List (intersperse)
import qualified Data.Map as M
import Control.Applicative ((<$>), (<*>))
import Control.Monad.RWS (RWST, asks, tell, get, put, lift)
import Polar.Asset.Shader.Types

data ShaderEnv = ShaderEnv
    { envFunctions      :: M.Map String [AST]
    , envInputs         :: M.Map String Int
    , envOutputs        :: M.Map String Int
    }
type ShaderState = ShaderType
type ShaderOutput = (String, String)
type ShaderM = RWST ShaderEnv ShaderOutput ShaderState (Either String)

unrecognized :: String -> ShaderM a
unrecognized name = lift $ Left ("unrecognized name (" ++ name ++ ")")

tellCurrent :: String -> ShaderM ()
tellCurrent msg = get >>= \case
    Vertex -> tell (msg, "")
    Pixel  -> tell ("", msg)

astComponents :: AST -> ShaderM Int
astComponents (Assignment lhs _) = astComponents lhs
astComponents (Swizzle asts) = foldr (+) 0 <$> mapM astComponents asts
astComponents (Literal _) = return 1
astComponents (Identifier name) = lift $ Left ("unresolved identifier (" ++ name ++ ")")
astComponents NamePosition = return 4
astComponents (NameInput name) = M.lookup name <$> asks envInputs >>= maybe (unrecognized name) return
astComponents (NameOutput name) = M.lookup name <$> asks envOutputs >>= maybe (unrecognized name) return

writeName :: String -> ShaderM ()
writeName name = get >>= \case
    Vertex -> case name of
        "position" -> tellCurrent "gl_Position"
        _          -> M.lookup name <$> asks envInputs >>= \case
            Just _  -> tellCurrent ("a_" ++ name)
            Nothing -> unrecognized name
    Pixel  -> M.lookup name <$> asks envOutputs >>= \case
        Just _  -> tellCurrent ("o_" ++ name)
        Nothing -> unrecognized name

writeAST :: AST -> ShaderM ()
writeAST (Assignment lhs rhs) = do
    tellCurrent "("
    writeAST lhs
    tellCurrent "="
    writeAST rhs
    tellCurrent ")"
    (==) <$> astComponents lhs <*> astComponents rhs >>= \case
        False -> lift (Left "number of components on lhs does not match number of components on rhs")
        True  -> return ()
writeAST ast@(Swizzle asts) = do
    components <- astComponents ast
    tellCurrent ("(vec" ++ show components ++ "(")
    sequence (tellCurrent "," `intersperse` map writeAST asts)
    tellCurrent "))"
writeAST (Literal literal) = tellCurrent (show literal)
writeAST (Identifier name) = lift $ Left ("unresolved identifier (" ++ name ++ ")")
writeAST NamePosition = tellCurrent "gl_Position"
writeAST (NameInput name) = tellCurrent ("a_" ++ name)
writeAST (NameOutput name) = tellCurrent ("o_" ++ name)

writeFunction :: String -> Maybe String -> ShaderM ()
writeFunction name mActualName = M.lookup name <$> asks envFunctions >>= \case
    Nothing -> unrecognized name
    Just asts -> do
        tellCurrent ("void " ++ fromMaybe name mActualName ++ "(){")
        sequence (tellCurrent ";" `intersperse` map writeAST asts)
        tellCurrent ";}"

writeIns :: ShaderM ()
writeIns = M.toList <$> asks envInputs >>= f 0
  where f _ [] = return ()
        f n ((x,c):xs) = do
            tell ("layout(location=" ++ show n ++ ")in vec" ++ show c ++ " a_" ++ x ++ ";", "")
            f (succ n) xs

writeOuts :: ShaderM ()
writeOuts = M.toList <$> asks envOutputs >>= f 0
  where f _ [] = return ()
        f n ((x,c):xs) = do
            tell ("", "out vec" ++ show c ++ " o_" ++ x ++ ";")
            f (succ n) xs

writeShaders :: ShaderM ()
writeShaders = do
    put Vertex
    tellCurrent (version ++ vExt ++ precision)
    writeIns
    writeFunction "vertex" (Just "main")
    put Pixel
    tellCurrent (version ++ precision)
    writeOuts
    writeFunction "pixel" (Just "main")
  where version = "#version 150\n"
        vExt = "#extension GL_ARB_explicit_attrib_location: enable\n"
        precision = "precision highp float;"

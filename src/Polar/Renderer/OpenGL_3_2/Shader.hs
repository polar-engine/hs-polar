{-# LANGUAGE LambdaCase #-}

module Polar.Renderer.OpenGL_3_2.Shader where

import Data.Maybe (fromMaybe)
import Data.List (nub, intercalate)
import qualified Data.Map as M
import Control.Applicative ((<$>), <*>)
import Control.Monad.State
import Polar.Asset.Shader.Tokenizer
import Polar.Asset.Shader.Parser
import Polar.Asset.Shader.Types

astComponents :: AST -> State ShaderEnv Int
astComponents (Assignment name _) = astComponents (Identifier name)
astComponents (Swizzle asts) = foldr (+) 0 <$> mapM astComponents asts
astComponents (Identifier name) = gets currentType >>= \case
    Just Vertex -> case name of
        "position" -> return 4
        _          -> M.lookup name <$> gets inputs >>= \case
            Just x  -> return x
            Nothing -> unrecognized
    Just Pixel  -> M.lookup name <$> gets outputs >>= \case
        Just x  -> return x
        Nothing -> unrecognized
    Nothing     -> unrecognized
  where unrecognized = fail ("unrecognized name (" ++ name ++ ")")
astComponents (Literal _) = return 1

showName :: String -> State ShaderEnv String
showName "position" = return "gl_Position"
showName name = gets currentType >>= \case
    Just Vertex -> case name of
        "position" -> return "gl_Position"
        _          -> M.lookup name <$> gets inputs >>= \case
            Just _  -> do
                modify (\env -> env { visitedInputs = name : visitedInputs env })
                return ("a_" ++ name)
            Nothing -> unrecognized
    Just Pixel  -> M.lookup name <$> gets outputs >>= \case
        Just _  -> do
            modify (\env -> env { visitedOutputs = name : visitedOutputs env })
            return ("o_" ++ name)
        Nothing -> unrecognized
    Nothing     -> unrecognized
  where unrecognized = fail ("unrecognized name (" ++ name ++ ")")

showAST :: AST -> State ShaderEnv String
showAST (Assignment name ast) = do
    rhs <- showAST ast
    lhs <- showAST (Identifier name)
    (==) <$> astComponents (Identifier name) <*> astComponents ast >>= \case
        False -> fail "number of components on lhs does not match number of components on rhs"
        True  -> return ('(' : lhs ++ '=' : rhs ++ ")")
showAST ast@(Swizzle asts) = do
    inner <- intercalate "," <$> mapM showAST asts
    components <- astComponents ast
    return ("(vec" ++ show components ++ '(' : inner ++ "))")
showAST (Identifier name) = showName name
showAST (Literal literal) = return (show literal)

showStatement :: AST -> State ShaderEnv String
showStatement ast = (++ ";") <$> showAST ast

showFunction :: String -> Maybe String -> State ShaderEnv String
showFunction name mActualName = M.lookup name <$> gets functions >>= \case
    Nothing -> fail ("unrecognized function name (" ++ name ++ ")")
    Just asts -> do
        statements <- mapM showStatement asts
        return ("void " ++ fromMaybe name mActualName ++ "(){" ++ concat statements ++ "}")

showIns :: State ShaderEnv [String]
showIns = nub <$> gets visitedInputs >>= f 0
  where f _ [] = return []
        f n (x:xs) = do
            c <- astComponents (Identifier x)
            rest <- f (succ n) xs
            return $ ("layout(location=" ++ show n ++ ")in vec" ++ show c ++ " a_" ++ x ++ ";") : rest

showOuts :: State ShaderEnv [String]
showOuts = nub <$> gets visitedOutputs >>= f 0
  where f _ [] = return []
        f n (x:xs) = do
            c <- astComponents (Identifier x)
            rest <- f (succ n) xs
            return $ ("out vec" ++ show c ++ " o_" ++ x ++ ";") : rest

showShaders :: State ShaderEnv (String, String)
showShaders = do
    modify (\env -> env { currentType = Just Vertex, visitedInputs = [] })
    vertex <- showFunction "vertex" (Just "main")
    ins <- concat <$> showIns
    modify (\env -> env { currentType = Just Pixel, visitedOutputs = [] })
    pixel <- showFunction "pixel" (Just "main")
    outs <- concat <$> showOuts
    return ( version ++ vExt ++ precision ++ ins ++ vertex
           , version         ++ precision ++ outs ++ pixel
           )
  where version = "#version 150\n"
        vExt = "#extension GL_ARB_explicit_attrib_location: enable\n"
        precision = "precision highp float;"

{-# LANGUAGE LambdaCase #-}

module Polar.Renderer.OpenGL_3_2.Shader where

import Data.Maybe (fromMaybe)
import Data.List (nub, intercalate)
import qualified Data.Map as M
import Control.Applicative ((<$>), (<*>))
import Control.Monad.RWS (asks, gets, modify, lift)
import Polar.Asset.Shader.Types

unrecognized :: String -> ShaderM a
unrecognized name = lift $ Left ("unrecognized name (" ++ name ++ ")")

astComponents :: AST -> ShaderM Int
astComponents (Assignment name _) = astComponents (Identifier name)
astComponents (Swizzle asts) = foldr (+) 0 <$> mapM astComponents asts
astComponents (Identifier name) = gets currentType >>= \case
    Just Vertex -> case name of
        "position" -> return 4
        _          -> M.lookup name <$> asks inputs >>= maybe (unrecognized name) return
    Just Pixel  -> M.lookup name <$> asks outputs >>= maybe (unrecognized name) return
    Nothing     -> unrecognized name
astComponents (Literal _) = return 1

showName :: String -> ShaderM String
showName name = gets currentType >>= \case
    Just Vertex -> case name of
        "position" -> return "gl_Position"
        _          -> M.lookup name <$> asks inputs >>= \case
            Just _  -> do
                modify (\env -> env { visitedInputs = name : visitedInputs env })
                return ("a_" ++ name)
            Nothing -> unrecognized name
    Just Pixel  -> M.lookup name <$> asks outputs >>= \case
        Just _  -> do
            modify (\env -> env { visitedOutputs = name : visitedOutputs env })
            return ("o_" ++ name)
        Nothing -> unrecognized name
    Nothing     -> unrecognized name

showAST :: AST -> ShaderM String
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

showFunction :: String -> Maybe String -> ShaderM String
showFunction name mActualName = M.lookup name <$> asks functions >>= \case
    Nothing -> unrecognized name
    Just asts -> do
        statements <- mapM showAST asts
        return ("void " ++ fromMaybe name mActualName ++ "(){" ++ concatMap (++ ";") statements ++ "}")

showIns :: ShaderM [String]
showIns = nub <$> gets visitedInputs >>= f 0
  where f _ [] = return []
        f n (x:xs) = do
            c <- astComponents (Identifier x)
            rest <- f (succ n) xs
            return $ ("layout(location=" ++ show n ++ ")in vec" ++ show c ++ " a_" ++ x ++ ";") : rest

showOuts :: ShaderM [String]
showOuts = nub <$> gets visitedOutputs >>= f 0
  where f _ [] = return []
        f n (x:xs) = do
            c <- astComponents (Identifier x)
            rest <- f (succ n) xs
            return $ ("out vec" ++ show c ++ " o_" ++ x ++ ";") : rest

showShaders :: ShaderM (String, String)
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

{-# LANGUAGE LambdaCase #-}

module Polar.Renderer.OpenGL_3_2.Shader where

import Data.Maybe (fromMaybe)
import Data.List (nub, intercalate)
import qualified Data.Map as M
import Control.Applicative ((<$>), (<*>))
import Control.Monad.RWS (RWST, asks, get, put, modify, lift)
import Polar.Asset.Shader.Types

data ShaderEnv = ShaderEnv
    { envFunctions      :: M.Map String [AST]
    , envInputs         :: M.Map String Int
    , envOutputs        :: M.Map String Int
    }
type ShaderState = Maybe ShaderType
type ShaderOutput = String
type ShaderM = RWST ShaderEnv () ShaderState (Either String)

unrecognized :: String -> ShaderM a
unrecognized name = lift $ Left ("unrecognized name (" ++ name ++ ")")

astComponents :: AST -> ShaderM Int
astComponents (Assignment name _) = astComponents (Identifier name)
astComponents (Swizzle asts) = foldr (+) 0 <$> mapM astComponents asts
astComponents (Identifier name) = get >>= \case
    Just Vertex -> case name of
        "position" -> return 4
        _          -> M.lookup name <$> asks envInputs >>= maybe (unrecognized name) return
    Just Pixel  -> M.lookup name <$> asks envOutputs >>= maybe (unrecognized name) return
    Nothing     -> unrecognized name
astComponents (Literal _) = return 1

showName :: String -> ShaderM String
showName name = get >>= \case
    Just Vertex -> case name of
        "position" -> return "gl_Position"
        _          -> M.lookup name <$> asks envInputs >>= \case
            Just _  -> return ("a_" ++ name)
            Nothing -> unrecognized name
    Just Pixel  -> M.lookup name <$> asks envOutputs >>= \case
        Just _  -> return ("o_" ++ name)
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
showFunction name mActualName = M.lookup name <$> asks envFunctions >>= \case
    Nothing -> unrecognized name
    Just asts -> do
        statements <- mapM showAST asts
        return ("void " ++ fromMaybe name mActualName ++ "(){" ++ concatMap (++ ";") statements ++ "}")

showIns :: ShaderM [String]
showIns = M.toList <$> asks envInputs >>= f 0
  where f _ [] = return []
        f n ((x,c):xs) = do
            rest <- f (succ n) xs
            return $ ("layout(location=" ++ show n ++ ")in vec" ++ show c ++ " a_" ++ x ++ ";") : rest

showOuts :: ShaderM [String]
showOuts = M.toList <$> asks envOutputs >>= f 0
  where f _ [] = return []
        f n ((x,c):xs) = do
            rest <- f (succ n) xs
            return $ ("out vec" ++ show c ++ " o_" ++ x ++ ";") : rest

showShaders :: ShaderM (String, String)
showShaders = do
    put (Just Vertex)
    vertex <- showFunction "vertex" (Just "main")
    ins <- concat <$> showIns
    put (Just Pixel)
    pixel <- showFunction "pixel" (Just "main")
    outs <- concat <$> showOuts
    return ( version ++ vExt ++ precision ++ ins ++ vertex
           , version         ++ precision ++ outs ++ pixel
           )
  where version = "#version 150\n"
        vExt = "#extension GL_ARB_explicit_attrib_location: enable\n"
        precision = "precision highp float;"

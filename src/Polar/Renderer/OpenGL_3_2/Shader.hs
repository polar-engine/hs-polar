{-# LANGUAGE LambdaCase #-}

module Polar.Renderer.OpenGL_3_2.Shader where

import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import qualified Data.Map as M
import Control.Monad.State
import Polar.Asset.Shader.Tokenizer
import Polar.Asset.Shader.Parser
import Polar.Asset.Shader.Types

data ShaderEnv = ShaderEnv
    { functions :: M.Map String [AST]
    }

defaultShaderEnv :: ShaderEnv
defaultShaderEnv = ShaderEnv
    { functions = M.empty
    }

astComponents' :: AST -> State ShaderEnv Int
astComponents' (Assignment name _) = astComponents' (Identifier name)
astComponents' (Swizzle asts) = liftM (foldr (+) 0) (mapM astComponents' asts)
astComponents' (Identifier "position") = return 4
astComponents' (Identifier name) = fail ("unrecognized name (" ++ name ++ ")")
--astComponents' (Identifier name) = case M.lookup name names of
--    Nothing -> Left ("unrecognized name (" ++ name ++ ")")
--    Just x  -> return x
astComponents' (Literal _) = return 1

resolveName' :: String -> State ShaderEnv String
resolveName' "position" = return "gl_Position"
resolveName' name = fail ("unrecognized name(" ++ name ++ ")")

showAST' :: AST -> State ShaderEnv String
showAST'(Assignment name ast) = do
    rhs <- showAST' ast
    lhs <- showAST' (Identifier name)
    liftM2 (==) (astComponents' (Identifier name)) (astComponents' ast) >>= \case
        False -> fail "number of components on lhs does not match number of components on rhs"
        True  -> return ('(' : lhs ++ '=' : rhs ++ ")")
showAST' ast@(Swizzle asts) = do
    inner <- liftM (intercalate ",") (mapM showAST' asts)
    components <- astComponents' ast
    return ("(vec" ++ show components ++ '(' : inner ++ "))")
showAST' (Identifier name) = resolveName' name
showAST' (Literal literal) = return (show literal)

showStatement' :: AST -> State ShaderEnv String
showStatement' ast = liftM (++ ";") (showAST' ast)

showFunction' :: String -> Maybe String -> State ShaderEnv String
showFunction' name mActualName = liftM (M.lookup name) (gets functions) >>= \case
    Nothing -> fail ("unrecognized function name (" ++ name ++ ")")
    Just asts -> do
        statements <- mapM showStatement' asts
        return ("void " ++ fromMaybe name mActualName ++ "(){" ++ concat statements ++ "}")

showShaders' :: State ShaderEnv (String, String)
showShaders' = do
    vertex <- showFunction' "vertex" (Just "main")
    pixel <- showFunction' "pixel" (Just "main")
    return (version ++ vertex, version ++ pixel)
  where version = "#version 150\n"

resolveName :: M.Map String Int -> String -> Either String (String, Int)
resolveName _ "position" = return ("gl_Position", 4)
resolveName names name = case M.lookup name names of
    Nothing -> Left ("unrecognized name (" ++ name ++ ")")
    Just x  -> return (name, x)

showAST :: M.Map String Int -> AST -> Either String String
showAST names (Assignment name ast) = do
    rhs <- showAST names ast
    name' <- liftM fst (resolveName names name)
    return ('(' : name' ++ '=' : rhs ++ ")")
showAST names ast@(Swizzle asts) = do
    inner <- liftM (intercalate ",") (mapM (showAST names) asts)
    components <- astComponents names ast
    return ("(vec" ++ show components ++ '(' : inner ++ "))")
showAST names (Identifier name) = liftM fst (resolveName names name)
showAST _ (Literal literal) = return (show literal)

showStatement :: M.Map String Int -> AST -> Either String String
showStatement names ast = liftM (++ ";") (showAST names ast)

showFunction :: M.Map String Int -> String -> [AST] -> Either String String
showFunction names name asts = do
    statements <- mapM (showStatement names) asts
    return (start ++ concat statements ++ end)
  where start = "void " ++ name ++ "(){"
        end = "}"

showShaders :: M.Map String Int -> M.Map String [AST] -> Either String (String, String)
showShaders names fns
    | M.notMember "vertex" fns = Left "shader does not have function `vertex`"
    | M.notMember "pixel" fns  = Left "shader does not have function `pixel`"
    | otherwise = do
        vertex <- showFunction names "main" (fns M.! "vertex")
        pixel <- showFunction names "main" (fns M.! "pixel")
        return (vertexHeader ++ vertex, pixelHeader ++ pixel)
  where vertexHeader = unlines
            [ "#version 150"
            , "#extension GL_ARB_explicit_attrib_location: enable"
            , "precision highp float;"
            ]
        pixelHeader = unlines
            [ "#version 150"
            , "precision highp float;"
            ]

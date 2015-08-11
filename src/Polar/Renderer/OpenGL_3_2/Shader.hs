module Polar.Renderer.OpenGL_3_2.Shader where

import Data.List (intercalate)
import qualified Data.Map as M
import Control.Monad (liftM)
import Polar.Asset.Shader.Tokenizer
import Polar.Asset.Shader.Parser
import Polar.Asset.Shader.Types

showAST :: M.Map String Integer -> AST -> Either String String
showAST names (Assignment name ast) = do
    rhs <- showAST names ast
    return ('(' : name ++ '=' : rhs ++ ")")
showAST names (Swizzle asts) = do
    inner <- liftM (intercalate ",") (mapM (showAST names) asts)
    return ("(vec(" ++ inner ++ "))")
showAST names (Identifier name)
    | M.member name names = return name
    | otherwise = Left "unrecognized name"
showAST _ (Literal literal) = return (show literal)

showStatement :: M.Map String Integer -> AST -> Either String String
showStatement names ast = liftM (++ ";") (showAST names ast)

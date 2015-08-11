module Polar.Renderer.OpenGL_3_2.Shader where

import Data.List (intercalate)
import qualified Data.Map as M
import Control.Monad (liftM)
import Polar.Asset.Shader.Tokenizer
import Polar.Asset.Shader.Parser
import Polar.Asset.Shader.Types

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

{-# LANGUAGE LambdaCase #-}

module Polar.Shader.Parser where

import qualified Data.Map as M
import Control.Applicative ((<$>))
import Polar.Shader.Types

parseAST :: [Token] -> Either String (AST, [Token])
parseAST [] = Left "unexpected end of stream"
parseAST (IdentifierT name : EqualsT : ts) = do
    (ast, rest) <- parseAST ts
    return (Assignment (Identifier name) ast, rest)
parseAST (IdentifierT x : ts) = case parseAST ts of
    Left _                     -> return (identifier, ts)
    Right (Swizzle asts, rest) -> return (Swizzle (identifier : asts), rest)
    Right (ast, rest)          -> return (Swizzle [identifier, ast], rest)
  where identifier = Identifier x
parseAST (LiteralT x : ts) = case parseAST ts of
    Left _                     -> return (literal, ts)
    Right (Swizzle asts, rest) -> return (Swizzle (literal : asts), rest)
    Right (ast, rest)          -> return (Swizzle [literal, ast], rest)
  where literal = Literal x
parseAST (t : _) = Left ("unexpected token (" ++ show t ++ ")")

parseStatements :: [Token] -> Either String ([AST], [Token])
parseStatements [] = return ([], [])
parseStatements (StatementEndT : ts) = parseStatements ts
parseStatements (NewLineT : ts) = parseStatements ts
parseStatements ts = do
    (ast, ts') <- parseAST ts
    (asts, rest) <- parseStatements ts'
    return (ast : asts, rest)

parseFunction :: [Token] -> Either String (String, [AST], [Token])
parseFunction [] = Left "unexpected end of stream"
parseFunction (IdentifierT name : BraceOpenT : ts)
    | null rest = Left ("unexpected end of stream")
    | otherwise = do
        (asts, []) <- parseStatements contents
        return (name, asts, tail rest)
  where (contents, rest) = break (== BraceCloseT) ts
parseFunction (t : NewLineT : ts) = parseFunction (t : ts)
parseFunction (t : _) = Left ("unexpected token (" ++ show t ++ ")")

parse :: [Token] -> Either String (M.Map String [AST])
parse [] = return M.empty
parse (NewLineT : ts) = parse ts
parse ts = do
    (name, asts, rest) <- parseFunction ts
    M.insertWith (flip const) name asts <$> parse rest

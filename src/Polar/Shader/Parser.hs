{-# LANGUAGE LambdaCase #-}

module Polar.Shader.Parser where

import qualified Data.Map as M
import Polar.Shader.Types


parse :: [Token] -> Either String (M.Map String [AST])
parse [] = return M.empty
parse (NewLineT : ts) = parse ts
parse ts = do
    (name, asts, rest) <- parseFunction ts
    M.insertWith (flip const) name asts <$> parse rest
parseFunction :: [Token] -> Either String (String, [AST], [Token])
parseFunction [] = Left "unexpected end of stream"
parseFunction (IdentifierT name : BraceOpenT : ts)
    | null rest = Left "unexpected end of stream"
    | otherwise = do
        (asts, []) <- parseStatements contents
        return (name, asts, tail rest)
  where (contents, rest) = break (== BraceCloseT) ts
parseFunction (t : NewLineT : ts) = parseFunction (t : ts)
parseFunction (t : _) = Left ("unexpected token (" ++ show t ++ ")")

parseStatements :: [Token] -> Either String ([AST], [Token])
parseStatements [] = return ([], [])
parseStatements (StatementEndT : ts) = parseStatements ts
parseStatements (NewLineT : ts) = parseStatements ts
parseStatements ts = do
    (ast, ts') <- parseAST ts
    (asts, rest) <- parseStatements ts'
    return (ast : asts, rest)

parseAST :: [Token] -> Either String (AST, [Token])
parseAST = parseAssignment

parseAssignment :: [Token] -> Either String (AST, [Token])
parseAssignment ts@(IdentifierT name : EqualsT : ts') = case parseAssignment ts' of
    Right (right, rest)         -> return (Assignment (Identifier name) right, rest)
    _                           -> parseAdditive ts
parseAssignment ts = parseAdditive ts

parseAdditive :: [Token] -> Either String (AST, [Token])
parseAdditive ts = case parseMultiplicative ts of
    Right (left, PlusT : ts') -> case parseAdditive ts' of
        Right (right, rest)       -> return (Additive left right, rest)
        _                         -> parseMultiplicative ts
    _                         -> parseMultiplicative ts

parseMultiplicative :: [Token] -> Either String (AST, [Token])
parseMultiplicative ts = case parsePrimary ts of
    Right (left, AsteriskT : ts') -> case parseMultiplicative ts' of
        Right (right, rest)           -> return (Multiplicative left right, rest)
        _                             -> parsePrimary ts
    _                             -> parsePrimary ts

parsePrimary :: [Token] -> Either String (AST, [Token])
parsePrimary [] = Left "unexpected end of stream"
parsePrimary (IdentifierT name : EqualsT : ts) = do
    (ast, rest) <- parsePrimary ts
    return (Assignment (Identifier name) ast, rest)
parsePrimary (IdentifierT x : ts) = case parsePrimary ts of
    Left _                     -> return (identifier, ts)
    Right (Swizzle asts, rest) -> return (Swizzle (identifier : asts), rest)
    Right (ast, rest)          -> return (Swizzle [identifier, ast], rest)
  where identifier = Identifier x
parsePrimary (LiteralT x : ts) = case parsePrimary ts of
    Left _                     -> return (literal, ts)
    Right (Swizzle asts, rest) -> return (Swizzle (literal : asts), rest)
    Right (ast, rest)          -> return (Swizzle [literal, ast], rest)
  where literal = Literal x
parsePrimary (t : _) = Left ("unexpected token (" ++ show t ++ ")")

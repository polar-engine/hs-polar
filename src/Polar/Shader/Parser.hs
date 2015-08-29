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
parseMultiplicative ts = case parseSwizzle ts of
    Right (left, AsteriskT : ts') -> case parseMultiplicative ts' of
        Right (right, rest)           -> return (Multiplicative left right, rest)
        _                             -> parseSwizzle ts
    _                             -> parseSwizzle ts

parseSwizzle :: [Token] -> Either String (AST, [Token])
parseSwizzle ts = case parsePrimary ts of
    Right (ast, ts') -> case parseSwizzle ts' of
        Right (Swizzle asts, rest) -> return (Swizzle (ast : asts), rest)
        Right (ast2, rest)         -> return (Swizzle [ast, ast2], rest)
        _                          -> parsePrimary ts
    _                -> parsePrimary ts

parsePrimary :: [Token] -> Either String (AST, [Token])
parsePrimary [] = Left "unexpected end of stream"
parsePrimary (IdentifierT name : ts) = return (Identifier name, ts)
parsePrimary (LiteralT literal : ts) = return (Literal literal, ts)
parsePrimary (BracketOpenT : ts) = case parseAST ts of
    Right (ast, BracketCloseT : rest) -> return (ast, rest)
    _                                 -> Left "no closing bracket"
parsePrimary (t : _) = Left ("unexpected token (" ++ show t ++ ")")

{-# LANGUAGE LambdaCase #-}

module Polar.Shader.Processor where

import qualified Data.Map as M
import Control.Monad.RWS (RWST, asks, tell, get, put, lift)
import Polar.Shader.Types

type ProcessorM = RWST CompilerEnv ([(String, DataType)], [(String, DataType)], [(String, DataType)]) ShaderType (Either String)

processAST :: AST -> ProcessorM AST
processAST (Additive (Literal left) (Literal right)) = processAST (Literal (left + right))
processAST (Multiplicative (Literal left) (Literal right)) = processAST (Literal (left * right))
processAST (Assignment left right) = do
    result <- Assignment <$> processAST left <*> processAST right
    lift (numComponents result)
    return result
processAST (Additive left right) = do
    result <- Additive <$> processAST left <*> processAST right
    lift (numComponents result)
    return result
processAST (Multiplicative left right) = do
    result <- Multiplicative <$> processAST left <*> processAST right
    lift (numComponents result)
    return result
processAST (Swizzle asts) = do
    result <- Swizzle <$> mapM processAST asts
    lift (numComponents result)
    return result
processAST ast@(Identifier name) = M.lookup name <$> asks compilerGlobals >>= \case
    Just x -> tell ([(name, x)], [], []) >> return (NameGlobal name x)
    Nothing -> get >>= \case
        ShaderVertex -> case name of
            "position" -> return NamePosition
            _          -> M.lookup name <$> asks compilerInputs >>= \case
                Just x  -> tell ([], [(name, x)], []) >> return (NameInput name x)
                Nothing -> return ast
        ShaderPixel  -> M.lookup name <$> asks compilerOutputs >>= \case
            Just x  -> tell ([], [], [(name, x)]) >> return (NameOutput name x)
            Nothing -> return ast
processAST ast = lift (numComponents ast) >> return ast

process :: ProcessorM (M.Map String [AST])
process = do
    fns <- asks compilerFunctions
    case M.lookup "vertex" fns of
        Nothing -> lift (Left "no vertex function")
        Just fn -> do
            put ShaderVertex
            vertex <- mapM processAST fn
            case M.lookup "pixel" fns of
                Nothing -> lift (Left "no pixel function")
                Just fn -> do
                    put ShaderPixel
                    pixel <- mapM processAST fn
                    return (M.insert "vertex" vertex (M.insert "pixel" pixel fns))

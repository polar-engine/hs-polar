{-# LANGUAGE LambdaCase #-}

module Polar.Shader.Processor where

import qualified Data.Map as M
import Control.Monad.RWS (RWST, asks, tell, get, put, lift)
import Polar.Shader.Types

type ProcessorM = RWST CompilerEnv ([(String, Int)], [(String, Int)]) Type (Either String)

processAST :: AST -> ProcessorM AST
processAST (Assignment lhs rhs) = processAST lhs >>= \ast -> Assignment ast <$> processAST rhs
processAST (Swizzle asts) = Swizzle <$> mapM processAST asts
processAST ast@(Identifier name) = get >>= \case
    Vertex -> case name of
        "position" -> return NamePosition
        _          -> M.lookup name <$> asks compilerInputs >>= \case
            Just x  -> tell ([(name, x)], []) >> return (NameInput name x)
            Nothing -> return ast
    Pixel  -> M.lookup name <$> asks compilerOutputs >>= \case
        Just x  -> tell ([], [(name, x)]) >> return (NameOutput name x)
        Nothing -> return ast
processAST ast = return ast

process :: ProcessorM (M.Map String [AST])
process = do
    fns <- asks compilerFunctions
    case M.lookup "vertex" fns of
        Nothing -> lift (Left "no vertex function")
        Just fn -> do
            put Vertex
            vertex <- mapM processAST fn
            case M.lookup "pixel" fns of
                Nothing -> lift (Left "no pixel function")
                Just fn -> do
                    put Pixel
                    pixel <- mapM processAST fn
                    return (M.insert "vertex" vertex (M.insert "pixel" pixel fns))

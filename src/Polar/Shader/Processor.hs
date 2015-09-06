{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Polar.Shader.Processor where

import qualified Data.Map as M
import Control.Monad.RWS (RWST, tell, get, put, lift)
import Control.Lens ((^.), view, at)
import Polar.Shader.Types

type ProcessorM = RWST CompilerEnv ([(String, DataType)], [(String, DataType)], [(String, DataType)]) ShaderType (Either String)

process :: ProcessorM (M.Map String Function)
process = do
    put ShaderVertex
    vertex <- processFunction "vertex"
    put ShaderPixel
    pixel <- processFunction "pixel"
    return $ M.fromList [("vertex", vertex), ("pixel", pixel)]

{- performs two passes
 - pass 1: process ASTs and resolve names
 - pass 2: infer let types and resolve names again
 -}
processFunction :: String -> ProcessorM Function
processFunction name = view (functions . at name) >>= \case
    Nothing -> lift $ Left ("function `" ++ name ++ "` does not exist")
    Just fn -> do
        asts <- processAST (M.fromList []) `mapM` (fn ^. asts)
        lets <- lift $ sequence [(name,) <$> astType ast | ast@(Let name _) <- asts]
        asts' <- processAST (M.fromList lets) `mapM` asts
        lift (mapM_ astType asts') -- type check
        return (Function name lets asts')

processAST :: M.Map String DataType -> AST -> ProcessorM AST
processAST lets (Additive       (Literal left) (Literal right)) = processAST lets (Literal (left + right))
processAST lets (Multiplicative (Literal left) (Literal right)) = processAST lets (Literal (left * right))
processAST lets (Let name            right) = Let name       <$> processAST lets right
processAST lets (Assignment     left right) = Assignment     <$> processAST lets left <*> processAST lets right
processAST lets (Additive       left right) = Additive       <$> processAST lets left <*> processAST lets right
processAST lets (Multiplicative left right) = Multiplicative <$> processAST lets left <*> processAST lets right
processAST lets (Swizzle asts) = Swizzle <$> mapM (processAST lets) asts
processAST lets ast@(Identifier name) = case M.lookup name lets of
    Just x  -> return (NameVar name x)
    Nothing -> view (globals . at name) >>= \case
        Just x  -> tell ([(name, x)], [], []) >> return (NameGlobal name x)
        Nothing -> get >>= \case
            ShaderVertex -> case name of
                "position" -> return NamePosition
                _          -> view (inputs . at name) >>= \case
                    Just x  -> tell ([], [(name, x)], []) >> return (NameInput name x)
                    Nothing -> return ast
            ShaderPixel  -> view (outputs . at name) >>= \case
                Just x  -> tell ([], [], [(name, x)]) >> return (NameOutput name x)
                Nothing -> return ast
processAST _ ast = lift (numComponents ast) >> return ast

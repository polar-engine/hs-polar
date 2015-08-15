{-# LANGUAGE LambdaCase #-}

module Polar.Asset.Shader.Processor where

import qualified Data.Map as M
import Control.Applicative ((<$>))
import Control.Monad.RWS (RWST, asks, tell, get, put, lift)
import Polar.Asset.Shader.Types (AST(..), ShaderType(..))

data ProcessorEnv = ProcessorEnv
    { envFunctions :: M.Map String [AST]
    , envInputs    :: M.Map String Int
    , envOutputs   :: M.Map String Int
    }
type ProcessorState = ShaderType
type ProcessorOutput = ([(String, Int)], [(String, Int)])
type ProcessorM = RWST ProcessorEnv ProcessorOutput ProcessorState (Either String)

processAST :: AST -> ProcessorM ()
processAST (Assignment name ast) = mapM_ processAST [Identifier name, ast]
processAST (Swizzle asts) = mapM_ processAST asts
processAST (Literal _) = return ()
processAST (Identifier name) = get >>= \case
    Vertex -> case name of
        "position" -> return ()
        _          -> M.lookup name <$> asks envInputs >>= \case
            Just x  -> tell ([(name, x)], [])
            Nothing -> return ()
    Pixel  -> M.lookup name <$> asks envOutputs >>= \case
        Just x  -> tell ([], [(name, x)])
        Nothing -> return ()

process :: ProcessorM ()
process = do
    fns <- asks envFunctions
    case M.lookup "vertex" fns of
        Nothing -> lift (Left "no vertex function")
        Just fn -> put Vertex >> mapM_ processAST fn
    case M.lookup "pixel" fns of
        Nothing -> lift (Left "no pixel function")
        Just fn -> put Pixel >> mapM_ processAST fn

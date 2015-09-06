{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Polar.Shader.Types where

import qualified Data.Map as M
import Control.Lens.TH (makeFields)

data Token = LetT
           | EqualsT
           | PlusT
           | AsteriskT
           | NewLineT
           | BracketOpenT
           | BracketCloseT
           | BraceOpenT
           | BraceCloseT
           | StatementEndT
           | IdentifierT String
           | LiteralT Double
             deriving (Eq, Show)

data AST = Let String AST
         | Assignment AST AST
         | Additive AST AST
         | Multiplicative AST AST
         | Swizzle [AST]
         | Literal Double
         | Identifier String
         | NamePosition
         | NameVar String DataType
         | NameGlobal String DataType
         | NameInput String DataType
         | NameOutput String DataType
           deriving (Eq, Show)

data ShaderType = ShaderVertex | ShaderPixel
data DataType = DataFloat | DataFloat2 | DataFloat4 | DataMatrix4x4 deriving (Eq, Show)

data Function = Function
    { _functionName :: String
    , _functionLets :: [(String, DataType)]
    , _functionAsts :: [AST]
    } deriving (Eq, Show)
makeFields ''Function

data CompilerEnv = CompilerEnv
    { _compilerEnvFunctions :: M.Map String Function
    , _compilerEnvGlobals   :: M.Map String DataType
    , _compilerEnvInputs    :: M.Map String DataType
    , _compilerEnvOutputs   :: M.Map String DataType
    }
makeFields ''CompilerEnv

class Compiler a where generate :: CompilerEnv -> a -> Either String (String, String)
class HasComponents a where numComponents :: a -> Either String Int

instance HasComponents DataType where
    numComponents DataFloat = return 1
    numComponents DataFloat2 = return 2
    numComponents DataFloat4 = return 4
    numComponents DataMatrix4x4 = return 16

instance HasComponents AST where
    numComponents (Let _ right) = numComponents right
    numComponents (Assignment left right) = do
        l <- numComponents left
        (l ==) <$> numComponents right >>= \case
            True  -> return l
            False -> Left "number of components on left does not match number of components on right"
    numComponents (Additive left right) = do
        l <- numComponents left
        (l ==) <$> numComponents right >>= \case
            True  -> return l
            False -> Left "number of components on left does not match number of components on right"
    numComponents (Multiplicative left right) = do
        l <- numComponents left
        r <- numComponents right
        if l == 16 && r == 4
            then return 4
            else (l ==) <$> numComponents right >>= \case
                True  -> return l
                False -> Left "number of components on left does not match number of components on right"
    numComponents (Swizzle []) = return 0
    numComponents (Swizzle (ast : asts)) = (+) <$> numComponents ast <*> numComponents (Swizzle asts)
    numComponents (Literal _) = return 1
    numComponents NamePosition = return 4
    numComponents (NameVar _ ty) = numComponents ty
    numComponents (NameGlobal _ ty) = numComponents ty
    numComponents (NameInput _ ty) = numComponents ty
    numComponents (NameOutput _ ty) = numComponents ty
    numComponents (Identifier name) = Left ("numComponents: unresolved identifier (" ++ name ++ ")")

astType :: AST -> Either String DataType
astType ast = numComponents ast >>= \case
    1  -> return DataFloat
    2  -> return DataFloat2
    4  -> return DataFloat4
    16 -> return DataMatrix4x4
    x  -> Left ("number of components (" ++ show x ++ ") does not match any supported data type")

module Polar.Asset.Shader.Types where

import qualified Data.Map as M
import Control.Monad.RWS (RWST)

data Token = EqualsT
           | NewLineT
           | BraceOpenT
           | BraceCloseT
           | StatementEndT
           | IdentifierT String
           | LiteralT Double
             deriving (Eq, Show)

data AST = Assignment String AST
         | Swizzle [AST]
         | Identifier String
         | Literal Double
           deriving Show

data ShaderType = Vertex | Pixel

data ShaderEnv = ShaderEnv
    { functions      :: M.Map String [AST]
    , inputs         :: M.Map String Int
    , outputs        :: M.Map String Int
    }

data ShaderState = ShaderState
    { currentType    :: Maybe ShaderType
    , visitedInputs  :: [String]
    , visitedOutputs :: [String]
    }

defaultShaderState :: ShaderState
defaultShaderState = ShaderState
    { currentType    = Nothing
    , visitedInputs  = []
    , visitedOutputs = []
    }

type ShaderM = RWST ShaderEnv () ShaderState (Either String)

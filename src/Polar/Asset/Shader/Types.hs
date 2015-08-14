module Polar.Asset.Shader.Types where

import qualified Data.Map as M
import Control.Monad.State (StateT)

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
    , currentType    :: Maybe ShaderType
    , inputs         :: M.Map String Int
    , outputs        :: M.Map String Int
    , visitedInputs  :: [String]
    , visitedOutputs :: [String]
    }

defaultShaderEnv :: ShaderEnv
defaultShaderEnv = ShaderEnv
    { functions      = M.empty
    , currentType    = Nothing
    , inputs         = M.empty
    , outputs        = M.empty
    , visitedInputs  = []
    , visitedOutputs = []
    }

type ShaderM = StateT ShaderEnv (Either String)

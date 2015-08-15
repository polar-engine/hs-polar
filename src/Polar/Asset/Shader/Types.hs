module Polar.Asset.Shader.Types where

import qualified Data.Map as M

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

module Polar.Asset.Shader.Types where

data Token = EqualsT
           | NewLineT
           | BraceOpenT
           | BraceCloseT
           | StatementEndT
           | IdentifierT String
           | LiteralT Double
             deriving (Eq, Show)

data AST = Assignment AST AST
         | Swizzle [AST]
         | Literal Double
         | Identifier String
         | NamePosition
         | NameInput String Int
         | NameOutput String Int
           deriving Show

data ShaderType = Vertex | Pixel

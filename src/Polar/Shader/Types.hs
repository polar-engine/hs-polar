module Polar.Shader.Types where

import Control.Applicative ((<$>))

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
           deriving (Eq, Show)

data Type = Vertex | Pixel

astComponents :: AST -> Either String Int
astComponents (Assignment lhs _) = astComponents lhs
astComponents (Swizzle asts) = foldr (+) 0 <$> mapM astComponents asts
astComponents (Literal _) = return 1
astComponents (Identifier name) = Left ("unresolved identifier (" ++ name ++ ")")
astComponents NamePosition = return 4
astComponents (NameInput _ x) = return x
astComponents (NameOutput _ x) = return x

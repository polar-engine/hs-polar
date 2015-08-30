module Polar.Shader.Types where

import qualified Data.Map as M

data Token = EqualsT
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

data AST = Assignment AST AST
         | Additive AST AST
         | Multiplicative AST AST
         | Swizzle [AST]
         | Literal Double
         | Identifier String
         | NamePosition
         | NameGlobal String Int
         | NameInput String Int
         | NameOutput String Int
           deriving (Eq, Show)

data Type = Vertex | Pixel

data CompilerEnv = CompilerEnv
    { compilerFunctions :: M.Map String [AST]
    , compilerGlobals   :: M.Map String Int
    , compilerInputs    :: M.Map String Int
    , compilerOutputs   :: M.Map String Int
    }

class Compiler a where generate :: CompilerEnv -> a -> Either String (String, String)

astComponents :: AST -> Either String Int
astComponents (Assignment left _) = astComponents left
astComponents (Additive left _) = astComponents left
astComponents (Multiplicative left _) = astComponents left
astComponents (Swizzle asts) = foldr (+) 0 <$> mapM astComponents asts
astComponents (Literal _) = return 1
astComponents (Identifier name) = Left ("unresolved identifier (" ++ name ++ ")")
astComponents NamePosition = return 4
astComponents (NameGlobal _ x) = return x
astComponents (NameInput _ x) = return x
astComponents (NameOutput _ x) = return x

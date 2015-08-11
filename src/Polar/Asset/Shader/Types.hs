module Polar.Asset.Shader.Types where

import qualified Data.Map as M
import Control.Monad (liftM)

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

astComponents :: M.Map String Int -> AST -> Either String Int
astComponents names (Assignment name _) = astComponents names (Identifier name)
astComponents names (Swizzle asts) = liftM (foldr (+) 0) (mapM (astComponents names) asts)
astComponents names (Identifier name) = case M.lookup name names of
    Nothing -> Left ("unrecognized name (" ++ name ++ ")")
    Just x  -> return x
astComponents _ (Literal _) = return 1

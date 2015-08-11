module Polar.Asset.Shader.Tokenizer where

import Data.Char (isAlphaNum)
import Control.Monad (liftM)
import Polar.Asset.Shader.Types

tokenize :: String -> Either String [Token]
tokenize [] = return []
tokenize ('=':xs) = liftM (EqualsT :) (tokenize xs)
tokenize ('{':xs) = liftM (BraceOpenT :) (tokenize xs)
tokenize ('}':xs) = liftM (BraceCloseT :) (tokenize xs)
tokenize (';':xs) = liftM (StatementEndT :) (tokenize xs)
tokenize (' ':xs) = tokenize xs
tokenize ('\n':xs) = liftM (NewLineT :) (tokenize xs)
tokenize ('\r':xs) = liftM (NewLineT :) (tokenize xs)
tokenize ('\t':xs) = tokenize xs
tokenize ('\v':xs) = tokenize xs
tokenize ('\f':xs) = tokenize xs
tokenize s@(x:_)
    | isAlphaNum x = case reads word :: [(Double, String)] of
        [(literal, "")] -> liftM (LiteralT literal :) (tokenize xs)
        _               -> liftM (IdentifierT word :) (tokenize xs)
    | otherwise = Left ("unrecognized token `" ++ x : "`")
  where (word, xs) = span isAlphaNum s

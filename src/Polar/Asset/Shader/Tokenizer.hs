module Polar.Asset.Shader.Tokenizer where

import Data.Char (isAlphaNum)
import Control.Monad (liftM)
import Polar.Asset.Shader.Types

tokenize :: String -> Either String [Token]
tokenize [] = return []
tokenize ('=':xs) = liftM (Equals :) (tokenize xs)
tokenize ('{':xs) = liftM (BraceOpen :) (tokenize xs)
tokenize ('}':xs) = liftM (BraceClose :) (tokenize xs)
tokenize (';':xs) = liftM (StatementEnd :) (tokenize xs)
tokenize (' ':xs) = tokenize xs
tokenize ('\n':xs) = tokenize xs
tokenize ('\r':xs) = tokenize xs
tokenize ('\t':xs) = tokenize xs
tokenize ('\v':xs) = tokenize xs
tokenize ('\f':xs) = tokenize xs
tokenize s@(x:_)
    | isAlphaNum x = case reads word :: [(Double, String)] of
        [(literal, "")] -> liftM (Literal literal :) (tokenize xs)
        _               -> liftM (Identifier word :) (tokenize xs)
    | otherwise = fail ("unexpected token `" ++ x : "`")
  where (word, xs) = span isAlphaNum s

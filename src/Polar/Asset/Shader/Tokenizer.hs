module Polar.Asset.Shader.Tokenizer where

import Data.Char (isAlphaNum)
import Control.Applicative ((<$>))
import Polar.Asset.Shader.Types

tokenize :: String -> Either String [Token]
tokenize [] = return []
tokenize ('=':xs) = (EqualsT :) <$> tokenize xs
tokenize ('{':xs) = (BraceOpenT :) <$> tokenize xs
tokenize ('}':xs) = (BraceCloseT :) <$> tokenize xs
tokenize (';':xs) = (StatementEndT :) <$> tokenize xs
tokenize (' ':xs) = tokenize xs
tokenize ('\n':xs) = (NewLineT :) <$> tokenize xs
tokenize ('\r':xs) = (NewLineT :) <$> tokenize xs
tokenize ('\t':xs) = tokenize xs
tokenize ('\v':xs) = tokenize xs
tokenize ('\f':xs) = tokenize xs
tokenize s@(x:_)
    | isAlphaNum x = case reads word :: [(Double, String)] of
        [(literal, "")] -> (LiteralT literal :) <$> tokenize xs
        _               -> (IdentifierT word :) <$> tokenize xs
    | otherwise = Left ("unrecognized token `" ++ x : "`")
  where (word, xs) = span isAlphaNum s

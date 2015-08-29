module Polar.Shader.Tokenizer where

import Data.Char (isAlphaNum)
import Polar.Shader.Types

isLiteralChar :: Char -> Bool
isLiteralChar '-' = True
isLiteralChar '.' = True
isLiteralChar '0' = True
isLiteralChar '1' = True
isLiteralChar '2' = True
isLiteralChar '3' = True
isLiteralChar '4' = True
isLiteralChar '5' = True
isLiteralChar '6' = True
isLiteralChar '7' = True
isLiteralChar '8' = True
isLiteralChar '9' = True
isLiteralChar _ = False

tokenize :: String -> Either String [Token]
tokenize [] = return []
tokenize ('=':xs) = (EqualsT :) <$> tokenize xs
tokenize ('+':xs) = (PlusT :) <$> tokenize xs
tokenize ('*':xs) = (AsteriskT :) <$> tokenize xs
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
    | isLiteralChar x || isAlphaNum x = case reads literalWord :: [(Double, String)] of
        [(literal, "")] -> (LiteralT literal :) <$> tokenize literalXs
        _               -> (IdentifierT word :) <$> tokenize xs
    | otherwise = Left ("unrecognized token `" ++ x : "`")
  where (literalWord, literalXs) = span isLiteralChar s
        (word, xs) = span isAlphaNum s

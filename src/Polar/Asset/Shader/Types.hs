module Polar.Asset.Shader.Types where

data Token = Equals
           | BraceOpen
           | BraceClose
           | StatementEnd
           | Identifier String
           | Literal Double
             deriving Show

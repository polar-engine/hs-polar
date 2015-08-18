module Polar.Test.Asset.Shader.Tokenizer where

import Test.HUnit
import qualified Polar.Asset.Shader.Types as Types
import qualified Polar.Asset.Shader.Tokenizer as Tokenizer

tests = TestList [ TestCase $ assertEqual "when input is empty" (Right []) (Tokenizer.tokenize "")
                 , TestCase $ assertEqual "when input is \"=\"" (Right [Types.EqualsT]) (Tokenizer.tokenize "=")
                 , TestCase $ assertEqual "when input is \"{\"" (Right [Types.BraceOpenT]) (Tokenizer.tokenize "{")
                 , TestCase $ assertEqual "when input is \"}\"" (Right [Types.BraceCloseT]) (Tokenizer.tokenize "}")
                 , TestCase $ assertEqual "when input is \";\"" (Right [Types.StatementEndT]) (Tokenizer.tokenize ";")
                 , TestCase $ assertEqual "when input is \" \"" (Right []) (Tokenizer.tokenize " ")
                 , TestCase $ assertEqual "when input is \"\\n\"" (Right [Types.NewLineT]) (Tokenizer.tokenize "\n")
                 , TestCase $ assertEqual "when input is \"\\r\"" (Right [Types.NewLineT]) (Tokenizer.tokenize "\r")
                 , TestCase $ assertEqual "when input is \"\\t\"" (Right []) (Tokenizer.tokenize "\t")
                 , TestCase $ assertEqual "when input is \"\\v\"" (Right []) (Tokenizer.tokenize "\v")
                 , TestCase $ assertEqual "when input is \"\\f\"" (Right []) (Tokenizer.tokenize "\f")
                 , TestCase $ assertEqual "when input is \"a\""   (Right [Types.IdentifierT "a"])   (Tokenizer.tokenize "a")
                 , TestCase $ assertEqual "when input is \"as\""  (Right [Types.IdentifierT "as"])  (Tokenizer.tokenize "as")
                 , TestCase $ assertEqual "when input is \"asd\"" (Right [Types.IdentifierT "asd"]) (Tokenizer.tokenize "asd")
                 , TestCase $ assertEqual "when input is \"1\""   (Right [Types.LiteralT 1.0]) (Tokenizer.tokenize "1")
                 , TestCase $ assertEqual "when input is \"1.0\"" (Right [Types.LiteralT 1.0]) (Tokenizer.tokenize "1.0")
                 ]

module Polar.Test.Asset.Shader.Tokenizer where

import Test.HUnit
import qualified Polar.Asset.Shader.Types as Types
import qualified Polar.Asset.Shader.Tokenizer as Tokenizer

testEmpty  = TestCase $ assertEqual "when input is empty" (Right []) (Tokenizer.tokenize "")
testEquals = TestCase $ assertEqual "when input is \"=\"" (Right [Types.EqualsT]) (Tokenizer.tokenize "=")

tests = TestList [ testEmpty
                 , testEquals
                 ]

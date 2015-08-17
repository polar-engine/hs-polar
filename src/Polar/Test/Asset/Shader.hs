module Polar.Test.Asset.Shader where

import Test.HUnit
import qualified Polar.Test.Asset.Shader.Tokenizer as Tokenizer

tests = TestList [ TestLabel "Tokenizer" Tokenizer.tests
                 ]

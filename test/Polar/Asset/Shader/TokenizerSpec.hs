module Polar.Asset.Shader.TokenizerSpec where

import Test.Hspec
import Polar.Asset.Shader.Tokenizer
import Polar.Asset.Shader.Types

spec = describe "Tokenizer" $ do
    it "returns an empty list when given an empty string" $
        tokenize ""
        `shouldBe` Right []
    it "returns an EqualsT when given an equals sign" $
        tokenize "="
        `shouldBe` Right [EqualsT]
    it "returns a BraceOpenT when given an opening curly brace" $
        tokenize "{"
        `shouldBe` Right [BraceOpenT]
    it "returns a BraceCloseT when given a closing curly brace" $
        tokenize "}"
        `shouldBe` Right [BraceCloseT]
    it "returns a StatementEndT when given a semicolon" $
        tokenize ";"
        `shouldBe` Right [StatementEndT]
    it "returns an empty list when given a space" $
        tokenize " "
        `shouldBe` Right []
    it "returns an empty list when given a horizontal tab character" $
        tokenize "\t"
        `shouldBe` Right []
    it "returns an empty list when given a vertical tab character" $
        tokenize "\v"
        `shouldBe` Right []
    it "returns an empty list when given a form feed character" $
        tokenize "\f"
        `shouldBe` Right []
    it "returns a NewLineT when given a newline character" $
        tokenize "\n"
        `shouldBe` Right [NewLineT]
    it "returns a NewLineT when given a carriage return character" $
        tokenize "\r"
        `shouldBe` Right [NewLineT]
    it "returns an IdentifierT \"a\" when given \"a\"" $
        tokenize "a"
        `shouldBe` Right [IdentifierT "a"]
    it "returns an IdentifierT \"as\" when given \"as\"" $
        tokenize "as"
        `shouldBe` Right [IdentifierT "as"]
    it "returns an IdentifierT \"asd\" when given \"asd\"" $
        tokenize "asd"
        `shouldBe` Right [IdentifierT "asd"]
    it "returns a LiteralT 1.0 when given \"1\"" $
        tokenize "1"
        `shouldBe` Right [LiteralT 1.0]
    it "returns a LiteralT 1.0 when given \"1.0\"" $
        tokenize "1.0"
        `shouldBe` Right [LiteralT 1.0]

module Polar.Asset.Shader.TokenizerSpec where

import Data.Either (isLeft)
import Test.Hspec
import Polar.Asset.Shader.Types
import Polar.Asset.Shader.Tokenizer

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
    it "returns a LiteralT 12.0 when given \"12\"" $
        tokenize "12"
        `shouldBe` Right [LiteralT 12.0]
    it "returns a LiteralT 123.0 when given \"123\"" $
        tokenize "123"
        `shouldBe` Right [LiteralT 123.0]
    it "returns a LiteralT 1.0 when given \"1.0\"" $
        tokenize "1.0"
        `shouldBe` Right [LiteralT 1.0]
    it "returns a LiteralT 12.0 when given \"12.0\"" $
        tokenize "12.0"
        `shouldBe` Right [LiteralT 12.0]
    it "returns a LiteralT 1.01 when given \"1.01\"" $
        tokenize "1.01"
        `shouldBe` Right [LiteralT 1.01]
    it "returns a LiteralT 12.01 when given \"12.01\"" $
        tokenize "12.01"
        `shouldBe` Right [LiteralT 12.01]
    it "returns a LiteralT -1.0 when given \"-1\"" $
        tokenize "-1"
        `shouldBe` Right [LiteralT (-1.0)]
    it "returns a LiteralT -1.0 when given \"-1.0\"" $
        tokenize "-1.0"
        `shouldBe` Right [LiteralT (-1.0)]
    it "returns a LiteralT -12.01 when given \"-12.01\"" $
        tokenize "-12.01"
        `shouldBe` Right [LiteralT (-12.01)]
    it "returns [EqualsT, BraceOpenT] when given \"={\"" $
        tokenize "={"
        `shouldBe` Right [EqualsT, BraceOpenT]
    it "returns [BraceOpenT, BraceCloseT] when given \"{ }\"" $
        tokenize "{ }"
        `shouldBe` Right [BraceOpenT, BraceCloseT]
    it "returns [BraceCloseT, NewLineT, NewLineT, StatementEndT, EqualsT] when given \"}  \\n\\r\\v\\t\\f;   =\"" $
        tokenize "}  \n\r\v\t\f;   ="
        `shouldBe` Right [BraceCloseT, NewLineT, NewLineT, StatementEndT, EqualsT]
    it "returns [IdentifierT \"asdf\", IdentifierT \"ghjkl\"] when given \"asdf ghjkl\"" $
        tokenize "asdf ghjkl"
        `shouldBe` Right [IdentifierT "asdf", IdentifierT "ghjkl"]
    it "returns [IdentifierT \"asdf\", LiteralT 12.01] when given \"asdf 12.01\"" $
        tokenize "asdf 12.01"
        `shouldBe` Right [IdentifierT "asdf", LiteralT 12.01]
    it "returns [LiteralT 12.01, IdentifierT \"asdf\"] when given \"12.01 asdf\"" $
        tokenize "12.01 asdf"
        `shouldBe` Right [LiteralT 12.01, IdentifierT "asdf"]
    it "returns an error when given a backtick" $
        tokenize "`"
        `shouldSatisfy` isLeft
    it "returns an error when given a backtick anywhere" $
        tokenize "asdf = 12.01`"
        `shouldSatisfy` isLeft

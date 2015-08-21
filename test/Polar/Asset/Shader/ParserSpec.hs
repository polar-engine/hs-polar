module Polar.Asset.Shader.ParserSpec where

import Data.Either (isLeft)
import qualified Data.Map as M
import Test.Hspec
import Polar.Asset.Shader.Types
import Polar.Asset.Shader.Parser

spec = describe "Parser" $ do
    it "returns an empty function map when given an empty list" $
        parse []
        `shouldBe` Right M.empty
    it "returns a singleton empty function when given \
             \[IdentifierT \"a\", BraceOpenT, BraceCloseT]" $
        parse [IdentifierT  "a" , BraceOpenT, BraceCloseT]
        `shouldBe` Right (M.singleton "a" [])
    it "returns two empty functions when given \
             \[IdentifierT \"a\", BraceOpenT, BraceCloseT, IdentifierT \"b\", BraceOpenT, BraceCloseT]" $
        parse [IdentifierT  "a" , BraceOpenT, BraceCloseT, IdentifierT  "b" , BraceOpenT, BraceCloseT]
        `shouldBe` Right (M.fromList
            [ ("a", [])
            , ("b", [])
            ])
    it "returns two functions with single ASTs when given \
             \[IdentifierT \"a\", BraceOpenT, LiteralT 1.0, BraceCloseT, IdentifierT \"b\", BraceOpenT, LiteralT 2.0, BraceCloseT]" $
        parse [IdentifierT  "a" , BraceOpenT, LiteralT 1.0, BraceCloseT, IdentifierT  "b" , BraceOpenT, LiteralT 2.0, BraceCloseT]
        `shouldBe` Right (M.fromList
            [ ("a", [Literal 1.0])
            , ("b", [Literal 2.0])
            ])
    it "returns a singleton function with a single AST when given \
             \[IdentifierT \"a\", BraceOpenT, LiteralT 1.0, BraceCloseT, IdentifierT \"a\", BraceOpenT, LiteralT 2.0, BraceCloseT]" $
        parse [IdentifierT  "a" , BraceOpenT, LiteralT 1.0, BraceCloseT, IdentifierT  "a" , BraceOpenT, LiteralT 2.0, BraceCloseT]
        `shouldBe` Right (M.singleton "a" [Literal 1.0])

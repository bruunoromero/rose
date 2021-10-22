{-# LANGUAGE OverloadedStrings #-}

module Rose.Parser.LanguageSpec where

import qualified Rose.Parser.Language as L
import Test.Hspec (Expectation, HasCallStack, Spec, describe, it, shouldBe)
import Test.Hspec.Megaparsec (initialState, shouldParse, succeedsLeaving)
import Test.Hspec.Megaparsec.Extra (parse, shouldFail, shouldSucceed, statefulParse)
import Text.Megaparsec (runParser')

spec :: Spec
spec = do
  describe "symbol" $ do
    it "should parse the string given as argument skipping trailing spaces" $ do
      parse (L.symbol "module") "module" `shouldParse` "module"
      parse (L.symbol "exposing") "exposing    " `shouldParse` "exposing"
      parse (L.symbol "IMPORT") "IMPORT" `shouldParse` "IMPORT"
      parse (L.symbol "   abc") "   abc       " `shouldParse` "   abc"
      parse (L.symbol "____") "____" `shouldParse` "____"
      parse (L.symbol "1234") "1234" `shouldParse` "1234"

    it "should not skip newlines" $ do
      statefulParse (L.symbol "exposing") "exposing\n    " `succeedsLeaving` "\n    "
      statefulParse (L.symbol "module") "module   \n    " `succeedsLeaving` "\n    "
      statefulParse (L.symbol "IMPORT") "IMPORT    \n" `succeedsLeaving` "\n"
      statefulParse (L.symbol "abc") "abc\n\n\n" `succeedsLeaving` "\n\n\n"

    it "should stop parsing on other characters" $ do
      statefulParse (L.symbol "exposing") "exposing all" `succeedsLeaving` "all"
      statefulParse (L.symbol "module") "module Main exposing" `succeedsLeaving` "Main exposing"
      statefulParse (L.symbol "import") "import\n A" `succeedsLeaving` "\n A"

    it "should fail to parse if the input do not start with the symbol" $ do
      shouldFail $ parse (L.symbol "exposing") "module"
      shouldFail $ parse (L.symbol "module") "\nmodule"
      shouldFail $ parse (L.symbol "IMPORT") "   IMPORT"

  describe "reserved" $ do
    it "should parse the symbol given as argument" $ do
      shouldSucceed $ parse (L.reserved "module") "module"
      shouldSucceed $ parse (L.reserved "exposing") "exposing    "
      shouldSucceed $ parse (L.reserved "IMPORT") "IMPORT"
      shouldSucceed $ parse (L.reserved "abc") "abc"
      shouldSucceed $ parse (L.reserved "____") "____"
      shouldSucceed $ parse (L.reserved "1234") "1234"

    it "should fail to parse the symbol given as argument" $ do
      shouldFail $ parse (L.reserved "module") "  module"
      shouldFail $ parse (L.reserved "import") "IMPORT"
      shouldFail $ parse (L.reserved "abc") "def"
      shouldFail $ parse (L.reserved "____") "----"
      shouldFail $ parse (L.reserved "1234") "4321"

  describe "number" $ do
    it "should parse an integer number" $ do
      parse L.number "0" `shouldParse` L.IntLiteral 0
      parse L.number "3" `shouldParse` L.IntLiteral 3
      parse L.number "10" `shouldParse` L.IntLiteral 10
      parse L.number "20" `shouldParse` L.IntLiteral 20
      parse L.number "123" `shouldParse` L.IntLiteral 123

    it "should parse a float number" $ do
      parse L.number "0.0" `shouldParse` L.FloatLiteral 0.0
      parse L.number "3.1" `shouldParse` L.FloatLiteral 3.1
      parse L.number "10.0" `shouldParse` L.FloatLiteral 10.0
      parse L.number "20.4" `shouldParse` L.FloatLiteral 20.4
      parse L.number "12.34567" `shouldParse` L.FloatLiteral 12.34567 
      statefulParse L.number "12.34567.0" `succeedsLeaving` ".0"

    it "should fail to parse what was given as argument" $ do
      shouldFail $ parse L.number "module"
      shouldFail $ parse L.number "-10"
      shouldFail $ parse L.number ""
      shouldFail $ parse L.number " "
      shouldFail $ parse L.number "-"
      shouldFail $ parse L.number "__"

  describe "identifier" $ do
    it "should parse an identifier" $ do
      parse L.identifier  "_" `shouldParse` L.Identifier "_"
      parse L.identifier  "d" `shouldParse` L.Identifier "d"
      parse L.identifier  "abc" `shouldParse` L.Identifier "abc"
      parse L.identifier  "_abc" `shouldParse` L.Identifier "_abc"
      parse L.identifier  "a123" `shouldParse` L.Identifier "a123"

    it "should fail parse an identifier" $ do
      shouldFail $ parse L.identifier ""
      shouldFail $ parse L.identifier " "
      shouldFail $ parse L.identifier "123a"
      shouldFail $ parse L.identifier "-abc"
      shouldFail $ parse L.identifier "?bcd"

  describe "parens" $ do
    it "should take a parser and returns what was parsed around parens" $ do
      parse (L.parens L.number) "(0)" `shouldParse` L.IntLiteral 0
      parse (L.parens L.number) "(0.0)" `shouldParse` L.FloatLiteral 0.0
      parse (L.parens L.identifier) "(abc)" `shouldParse` L.Identifier "abc"

    it "should fail to parse what was given as argument" $ do
      shouldFail $ parse (L.parens L.number) "0"
      shouldFail $ parse (L.parens L.number) "0.0"
      shouldFail $ parse (L.parens L.identifier) "abc"

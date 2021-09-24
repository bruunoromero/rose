{-# LANGUAGE OverloadedStrings #-}

module Rose.Parser.LanguageSpec where

import qualified Rose.Parser.Language as L
import Test.Hspec (Expectation, HasCallStack, Spec, describe, it, shouldBe)
import Test.Hspec.Megaparsec (initialState, shouldParse, succeedsLeaving)
import TestHelper (parse, shouldFail, shouldSucceed, statefulParse)
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

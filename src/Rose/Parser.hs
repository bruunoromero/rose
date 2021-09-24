{-# LANGUAGE OverloadedStrings #-}

module Rose.Parser where

import Data.Either.Combinators (mapRight)
import Data.Text as T (Text, pack)
import Data.Void (Void)
import Flow (compose, (|>))
import qualified Rose.Parser.Ast.Definition as D
import qualified Rose.Parser.Ast.Module as SM
import qualified Rose.Parser.Language as L
import Text.Megaparsec as P (ParseErrorBundle, between, endBy, many, parseTest, runParser)
import Text.Megaparsec.Error (ParseErrorBundle)

data File = File
  { sourceName :: String
  , program :: Program
  }
  deriving (Show, Eq)

data Program = Program
  { module_ :: SM.Module
  , definitions :: [D.Definition]
  }
  deriving (Show, Eq)

parseProgram :: L.Parser Program
parseProgram = L.betweenNl $ do
  module_ <- SM.parse
  definitions <- P.endBy D.parse L.nl

  return
    Program
      { module_ = module_
      , definitions = definitions
      }

parse :: String -> String -> Either (ParseErrorBundle Text Void) File
parse sourceName input =
  T.pack input
    |> runParser parseProgram sourceName
    |> mapRight (File sourceName)

parseIO :: String -> IO ()
parseIO input =
  T.pack input
    |> parseTest parseProgram

parseNumber :: Text -> String -> IO ()
parseNumber sourceName input =
  input
    |> T.pack
    |> parseTest D.parse

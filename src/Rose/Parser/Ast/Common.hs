{-# LANGUAGE OverloadedStrings #-}

module Rose.Parser.Ast.Common where

import qualified Rose.Parser.Language as L
import Text.Megaparsec (choice)
import qualified Text.Megaparsec as P

type ModuleName = [L.Identifier]

parseModuleName :: L.Parser [L.Identifier]
parseModuleName = P.sepBy1 L.identifier (L.reservedOp "::")

data Exposing
  = All
  | Pick [L.Identifier]
  deriving (Show, Eq)

parseExposing :: L.Parser Exposing
parseExposing =
  L.reserved "exposing" <* L.nl
    *> choice
      [ parseAll
      , parsePick
      ]
 where
  parseAll = All <$ L.reserved "all"
  parsePick = Pick <$> L.parens (L.commaSep1 L.identifier)

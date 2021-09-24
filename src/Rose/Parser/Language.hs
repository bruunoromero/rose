{-# LANGUAGE OverloadedStrings #-}

module Rose.Parser.Language where

import Control.Monad (void)
import Data.Functor (($>))
import qualified Data.Scientific as DS
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, (<?>), (<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as PC
import qualified Text.Megaparsec.Char.Lexer as L

newtype Identifier
  = Identifier String
  deriving (Show, Eq)

newtype Operator
  = Operator String
  deriving (Show, Eq)

newtype StringLiteral
  = StringLiteral String
  deriving (Show, Eq)

data Number
  = IntLiteral Integer
  | FloatLiteral Double
  deriving (Show, Eq)

type Parser = Parsec Void Text

wrapNumber :: Either Double Integer -> Number
wrapNumber (Left f) = FloatLiteral f
wrapNumber (Right i) = IntLiteral i

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

whitescpace :: Parser ()
whitescpace = L.space (void $ P.some (PC.char ' ' <|> PC.char '\t')) lineComment P.empty

nl :: Parser ()
nl = L.space PC.space1 lineComment P.empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whitescpace

lexemeNl :: Parser a -> Parser a
lexemeNl = L.lexeme nl

symbol :: Text -> Parser Text
symbol = L.symbol whitescpace

betweenNl :: Parser a -> Parser a
betweenNl = P.between nl nl

number :: Parser Number
number = wrapNumber . DS.floatingOrInteger <$> lexeme L.scientific

opStart :: Parser Char
opStart = opLetter

opLetter :: Parser Char
opLetter =
  P.oneOf
    [ ':'
    , '!'
    , '#'
    , '$'
    , '%'
    , '&'
    , '*'
    , '+'
    , '.'
    , '/'
    , '<'
    , '='
    , '>'
    , '?'
    , '@'
    , '\\'
    , '^'
    , '|'
    , '-'
    , '~'
    ]

identStart :: Parser Char
identStart = (PC.alphaNumChar <|> PC.char '_') <?> "identStart"

identLetter :: Parser Char
identLetter = (PC.alphaNumChar <|> P.oneOf ['_']) <?> "identLetter"

followedBy :: Parser Char -> Parser Char -> Parser String
followedBy start following = do
  head <- start
  tail <- P.many following

  return (head : tail)

identifier :: Parser Identifier
identifier = Identifier <$> lexeme (followedBy identStart identLetter)

reserved :: Text -> Parser ()
reserved keyword = symbol keyword $> ()

operator :: Parser String
operator = P.some opLetter

reservedOp :: Text -> Parser ()
reservedOp op = symbol op $> ()

parens :: Parser a -> Parser a
parens = P.between (symbol "(") (symbol ")")

commaSep :: Parser a -> Parser [a]
commaSep parser = P.sepBy parser (symbol ",")

commaSep1 :: Parser a -> Parser [a]
commaSep1 parser = P.sepBy1 parser (symbol ",")

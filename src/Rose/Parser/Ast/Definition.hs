{-# LANGUAGE OverloadedStrings #-}

module Rose.Parser.Ast.Definition where

import qualified Rose.Parser.Ast.Expr as Expr
import qualified Rose.Parser.Language as L
import qualified Text.Megaparsec as P

data Definition = Definition
  { name :: L.Identifier
  , args :: [L.Identifier]
  , body :: L.Number
  }
  deriving (Show, Eq)

parse :: L.Parser Definition
parse = do
  name <- L.identifier
  args <- P.many L.identifier
  L.reservedOp "="
  body <- Expr.parse

  return
    Definition
      { name = name
      , args = args
      , body = body
      }
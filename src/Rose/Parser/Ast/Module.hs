{-# LANGUAGE OverloadedStrings #-}

module Rose.Parser.Ast.Module where

import qualified Rose.Parser.Ast.Common as Common
import qualified Rose.Parser.Language as L
import qualified Text.Megaparsec as P

data Module = Module
  { name :: Common.ModuleName
  , exposing :: Common.Exposing
  }
  deriving (Show, Eq)

parse :: L.Parser Module
parse = do
  L.reserved "module" <* L.nl
  name <- Common.parseModuleName
  exposing <- Common.parseExposing

  return
    Module
      { name = name
      , exposing = exposing
      }
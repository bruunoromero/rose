{-# LANGUAGE OverloadedStrings #-}

module Rose.Parser.Ast.Expr where

import GHC.Show (Show)
import qualified Rose.Parser.Ast.Common as Common
import qualified Rose.Parser.Language as L

parse :: L.Parser L.Number
parse = L.number

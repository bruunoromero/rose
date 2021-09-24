{-# LANGUAGE OverloadedStrings #-}

module Main where

import Flow ((|>))
import qualified Rose.Parser as Parser
import Text.Pretty.Simple (pPrint)

fileName :: String
fileName = "./example.rose"

main :: IO ()
main = do
  content <- readFile "./example.rose"
  Parser.parseIO content

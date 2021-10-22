module Test.Hspec.Megaparsec.Extra where

import Test.Hspec (Expectation, HasCallStack, expectationFailure)
import Test.Hspec.Megaparsec (initialState, parseSatisfies)
import Text.Megaparsec (ParseErrorBundle, Stream, TraversableStream, VisualStream, errorBundlePretty, runParser')
import qualified Text.Megaparsec as P
import Text.Megaparsec.Error (ShowErrorComponent)

parse :: P.Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parse parser = P.parse parser ""

statefulParse :: P.Parsec e s a -> s -> (P.State s e, Either (ParseErrorBundle s e) a)
statefulParse parser input = runParser' parser (initialState input)

showBundle ::
    ( ShowErrorComponent e
    , Stream s
    , VisualStream s
    , TraversableStream s
    ) =>
    ParseErrorBundle s e ->
    String
showBundle = unlines . fmap indent . lines . errorBundlePretty
  where
    indent x =
        if null x
            then x
            else "  " ++ x

shouldFail ::
    (HasCallStack, Show a) =>
    Either (ParseErrorBundle s e) a ->
    Expectation
shouldFail r = case r of
    Left _ -> return ()
    Right v ->
        expectationFailure $
            "the parser is expected to fail, but it parsed: " ++ show v

shouldSucceed ::
    ( HasCallStack
    , ShowErrorComponent e
    , Stream s
    , VisualStream s
    , TraversableStream s
    , Show a
    ) =>
    Either (ParseErrorBundle s e) a ->
    Expectation
shouldSucceed r = case r of
    Left e ->
        expectationFailure $
            "the parser is expected to succeed, but it failed with:\n"
                ++ showBundle e
    Right _ -> return ()

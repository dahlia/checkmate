{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Checkmate.Parser.IndentBlockSpec (spec) where

import System.IO (hClose)

import Data.FileEmbed
import Data.Text
import Data.Text.IO
import Text.InterpolatedString.Perl6
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec hiding (ParseError)
import System.IO.Temp

import Checkmate.Check
import Checkmate.Parser.IndentBlock
import Checkmate.Range

pythonFixture :: Text
pythonFixture = $(embedStringFile "test/fixtures/sample.py")

spec :: Spec
spec = do
    parserSpec "parser" $ \ text -> do
        let path = "sample.txt"
        return (path, parse parser path text)
    parserSpec "parseSourceCode" $ \ text -> do
        let path = "sample.txt"
        return (path, parseSourceCode path text)
    parserSpec "parseSourceFile" $ \ text ->
        withSystemTempFile "checkmate-test" $ \ filePath handle -> do
            hPutStr handle text
            hClose handle
            parsed <- parseSourceFile filePath
            return (filePath, parsed)
    it "parses a multiline comment" $ do
        let parsed = parse parser "a.js" [q|
function foo() {
    /*
    CHECK multiline
    test
    test2
    */
    return true;
|]
        parsed `shouldParse`
            [ Check (FileBlock "a.js" $ Range 3 6) 1
                    "multiline\ntest\ntest2"
            ]

parserSpec :: String
           -> (Text -> IO (FilePath, Either ParseError Checklist))
           -> Spec
parserSpec specName parse' = describe specName $ do
    it "returns an empty set if a given text is empty" $ do
        (_, parsed) <- parse' ""
        parsed `shouldParse` []
    it "parses test/fixtures/sample.py" $ do
        (path, parsed) <- parse' pythonFixture
        let s = FileBlock path
        parsed `shouldParse`
            [ Check (s $ Range 2 24) 1 "module-level check"
            , Check (s $ Range 6 2) 2 "function-level check"
            , Check (s $ Range 11 5) 3 "function-level check 2"
            , Check (s $ Range 14 2) 4 "nested function-level check"
            , Check (s $ Range 19 7) 5 "class-level check"
            , Check (s $ Range 23 3) 6
                    "method-level check.\nIt can be multiline."
            ]

{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Checkmate.Parser.IndentBlockSpec (spec) where

import System.IO (hClose)

import Data.FileEmbed
import Data.Range.Range
import Data.Text
import Data.Text.IO
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import System.IO.Temp

import Checkmate.Check
import Checkmate.Parser.IndentBlock

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

parserSpec
    :: String
    -> (Text -> IO (FilePath, Either (ParseError (Token Text) Dec) Checklist))
    -> Spec
parserSpec specName parse' = describe specName $ do
    it "returns an empty set if a given text is empty" $ do
        (_, parsed) <- parse' ""
        parsed `shouldParse` []
    it "parses test/fixtures/sample.py" $ do
        (path, parsed) <- parse' pythonFixture
        let s = FileBlock path
        parsed `shouldParse`
            [ Check (s $ SpanRange 2 23) 1 "module-level check"
            , Check (s $ SpanRange 6 7) 2 "function-level check"
            , Check (s $ SpanRange 11 15) 3 "function-level check 2"
            , Check (s $ SpanRange 14 15) 4 "nested function-level check"
            , Check (s $ SpanRange 19 23) 5 "class-level check"
            , Check (s $ SpanRange 22 23) 6 "method-level check"
            ]

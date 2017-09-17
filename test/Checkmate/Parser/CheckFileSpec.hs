{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Checkmate.Parser.CheckFileSpec (spec) where

import Control.Monad
import System.IO (hClose)

import Data.Text
import Data.Text.IO
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.InterpolatedString.Perl6
import Text.Megaparsec
import System.FilePath
import System.IO.Temp

import Checkmate.Check
import Checkmate.Parser.CheckFile

spec :: Spec
spec = do
    parserSpec "parser" $ \ text ->
        return (".", parse (parser ".") ("." </> "CHECK") text)
    parserSpec "parseCheckFileText" $ \ text ->
        return (".", parseCheckFileText ("." </> "CHECK") text)
    parserSpec "parseCheckFileText" $ \ text ->
        withSystemTempFile "checkmate-test" $ \ filePath handle -> do
            hPutStr handle text
            hClose handle
            result <- parseCheckFile filePath
            return (takeDirectory filePath, result)

parserSpec :: String
           -> ( Text -> IO ( FilePath
                           , Either (ParseError (Token Text) Dec) Checklist
                           )
              )
           -> Spec
parserSpec specName parseFn =
    describe specName $ do
        it "returns [] if a given text is empty" $ do
            (_, parsed) <- parse' ""
            parsed `shouldParse` []
        let bulletStyles =
                [ ("CHECK", "CHECK", "CHECK", "CHECK")
                , ("bullet \"*\"", "*", "*", "*")
                , ("bullet \"-\"", "-", "-", "-")
                , ("bullet \"+\"", "+", "+", "+")
                , ("digit \"1.\"", "1.", "2.", "3.")
                , ("digit \"1)\"", "1)", "2)", "3)")
                ] :: [(Text, Text, Text, Text)]
        forM_ bulletStyles $ \ (styleName, b1, b2, b3) -> do
            it [qq|interprets $styleName line as a Check|] $ do
                (dir, parsed) <- parse' [qq|$b1 foo|]
                parsed `shouldParse` [Check dir 1 "foo"]
            let nl = "\n" :: Text
            it [qq|interprets multiple $styleName lines as [Check]|] $ do
                (ad, a) <- parse' [qq|$b1 foo$nl$b2 bar$nl|]
                a `shouldParse` [Check ad 1 "foo", Check ad 2 "bar"]
                (bd, b) <- parse' [qq|$b1 foo$nl$b2 bar$nl$b3 baz|]
                b `shouldParse` [ Check bd 1 "foo"
                                , Check bd 2 "bar"
                                , Check bd 3 "baz"
                                ]
            it [qq|interprets multiple lines as a Check until it's followed 
                   by another $styleName|] $ do
                (ad, a) <- parse' [qq|$b1  foo{nl}bar{nl}qux$nl|]
                a `shouldParse` [Check ad 1 "foo\nbar\nqux"]
                (bd, b) <- parse' [qq|$b1 foo{nl}bar$nl$b2 qux$nl|]
                b `shouldParse` [Check bd 1 "foo\nbar", Check bd 2 "qux"]
            it "ignores the leading spaces/empty lines" $ do
                (_, a) <- parse' "    "
                a `shouldParse` []
                (_, b) <- parse' "\t"
                b `shouldParse` []
                (_, c) <- parse' "\n"
                c `shouldParse` []
                (_, d) <- parse' "\r\n"
                d `shouldParse` []
                (_, e) <- parse' "\n\n"
                e `shouldParse` []
                (_, f) <- parse' "  \t  \n\n"
                f `shouldParse` []
                (dir, g) <- parse' (mconcat ["  \t  \n\n", b1, " foo"])
                g `shouldParse` [Check dir 1 "foo"]
        it "parses well even if bullet styles are mixed" $ do
            (dir, parsed) <- parse'
                "CHECK foo\nbar\n- baz\nqux\n* quz\n+ a\n1. b\n2) c\nCHECK d"
            parsed `shouldParse`
                [ Check dir 1 "foo\nbar"
                , Check dir 2 "baz\nqux"
                , Check dir 3 "quz"
                , Check dir 4 "a"
                , Check dir 5 "b"
                , Check dir 6 "c"
                , Check dir 7 "d"
                ]
  where
    parse' :: Text -> IO ( Scope
                         , Either (ParseError (Token Text) Dec) Checklist
                         )
    parse' text = do
        (dir, result) <- parseFn text
        return (Directory dir, result)

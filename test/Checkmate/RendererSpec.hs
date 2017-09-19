{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Checkmate.RendererSpec where

import Data.Range.Range
import Data.Text
import System.FilePath
import Test.Hspec
import Text.InterpolatedString.Perl6

import Checkmate.Check
import Checkmate.Renderer

fixture :: Checklist
fixture =
    [ Check (Directory $ "b" </> "foo" </> "bar" </> ".") 1
            "Lorem ipsum dolor sit amet,"
    , Check (Directory $ "b" </> "foo" </> "bar" </> ".") 2
            "consectetur adipiscing elit,"
    , Check (Directory $ "b" </> "foo" </> "bar" </> ".") 3
            "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."
    , Check (FileBlock ("b" </> "foo" </> "bar" </> "baz.c") $ SpanRange 8 21) 1
            "Ut enim ad minim veniam,"
    , Check (FileBlock ("b" </> "foo" </> "bar" </> "baz.c") $ SpanRange 9 21) 2 $
            "quis nostrud exercitation ullamco laboris nisi ut\n" `append`
            "aliquip ex ea commodo consequat."
    ]

thinkingFace :: Text
thinkingFace = "\x1f914"

spec :: Spec
spec = do
    specify "toCommonMark" $
        toCommonMark "b" 1 fixture `shouldBe` [qq|# Checklist  $thinkingFace

## `foo/bar/`

 -  Lorem ipsum dolor sit amet,
 -  consectetur adipiscing elit,
 -  sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.

## `foo/bar/baz.c`

 -  Ut enim ad minim veniam,
 -  quis nostrud exercitation ullamco laboris nisi ut
    aliquip ex ea commodo consequat.
|]
    specify "toGFMarkdown" $
        toGFMarkdown "b" 3 fixture `shouldBe` [qq|### Checklist  $thinkingFace

#### `foo/bar/`

 -  [ ] Lorem ipsum dolor sit amet,
 -  [ ] consectetur adipiscing elit,
 -  [ ] sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.

#### `foo/bar/baz.c`

 -  [ ] Ut enim ad minim veniam,
 -  [ ] quis nostrud exercitation ullamco laboris nisi ut
    aliquip ex ea commodo consequat.
|]

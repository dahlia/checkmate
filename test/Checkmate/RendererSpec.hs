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
    [ Check (Directory $ "foo" </> "bar") 1
            "Lorem ipsum dolor sit amet,"
    , Check (Directory $ "foo" </> "bar") 2
            "consectetur adipiscing elit,"
    , Check (Directory $ "foo" </> "bar") 3
            "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."
    , Check (FileBlock ("foo" </> "bar" </> "baz.c") $ SpanRange 8 21) 1
            "Ut enim ad minim veniam,"
    , Check (FileBlock ("foo" </> "bar" </> "baz.c") $ SpanRange 9 21) 2 $
            "quis nostrud exercitation ullamco laboris nisi ut\n" `append`
            "aliquip ex ea commodo consequat."
    ]

thinkingFace :: Text
thinkingFace = "\x1f914"

spec :: Spec
spec = do
    specify "toCommonMark" $
        toCommonMark fixture `shouldBe` [qq|### Checklist  $thinkingFace

 -  Lorem ipsum dolor sit amet,
 -  consectetur adipiscing elit,
 -  sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
 -  Ut enim ad minim veniam,
 -  quis nostrud exercitation ullamco laboris nisi ut
    aliquip ex ea commodo consequat.
|]
    specify "toGFMarkdown" $
        toGFMarkdown fixture `shouldBe` [qq|### Checklist  $thinkingFace

 -  [ ] Lorem ipsum dolor sit amet,
 -  [ ] consectetur adipiscing elit,
 -  [ ] sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
 -  [ ] Ut enim ad minim veniam,
 -  [ ] quis nostrud exercitation ullamco laboris nisi ut
    aliquip ex ea commodo consequat.
|]

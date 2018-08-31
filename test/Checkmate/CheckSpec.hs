{-# LANGUAGE FlexibleInstances #-}
module Checkmate.CheckSpec (spec) where

import System.FilePath
import Test.Hspec

import Checkmate.Check
import Checkmate.Range

spec :: Spec
spec =
    describe "Scope" $ do
        let p = "foo" </> "bar"
            r = Range 1 1
        it "doesn't equal if two values' data constructors are different" $ do
            FileBlock p r `shouldNotBe` Directory p
            FileBlock p r `shouldNotBe` Directory (p </> "baz")
        it "doesn't equal if two FileBlocks have different scopeRange" $ do
            FileBlock p r `shouldNotBe` FileBlock p (Range 2 1)
            FileBlock p r `shouldNotBe` FileBlock (p </> "baz") (Range 2 1)
        it "equals if two values' scopePaths refer to the same path" $ do
            Directory p `shouldBe` Directory p
            FileBlock p r `shouldBe` FileBlock p r
            Directory p `shouldBe` Directory ("." </> p)
            FileBlock p r `shouldBe` FileBlock ("." </> p) r

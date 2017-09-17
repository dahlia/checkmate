{-# LANGUAGE FlexibleInstances #-}
module Checkmate.CheckSpec (spec) where

import Data.Range.Range
import System.FilePath
import Test.Hspec

import Checkmate.Check

spec :: Spec
spec =
    describe "Scope" $ do
        let p = "foo" </> "bar"
            r = SingletonRange 1
        it "doesn't equal if two values' data constructors are different" $ do
            FileBlock p r `shouldNotBe` Directory p
            FileBlock p r `shouldNotBe` Directory (p </> "baz")
        it "doesn't equal if two FileBlocks have different scopeRange" $ do
            FileBlock p r `shouldNotBe` FileBlock p (SingletonRange 2)
            FileBlock p r `shouldNotBe`
                FileBlock (p </> "baz") (SingletonRange 2)
        it "equals if two values' scopePaths refer to the same path" $ do
            Directory p `shouldBe` Directory p
            FileBlock p r `shouldBe` FileBlock p r
            Directory p `shouldBe` Directory ("." </> p)
            FileBlock p r `shouldBe` FileBlock ("." </> p) r

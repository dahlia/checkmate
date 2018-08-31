module Checkmate.RangeSpec where

import Test.Hspec

import Checkmate.Range

spec :: Spec
spec =
    specify "rangesOverlap" $ do
        Range 7 10 `shouldSatisfy` rangesOverlap (Range 5 5)
        Range 5 5 `shouldSatisfy` rangesOverlap (Range 7 10)
        Range 3 3 `shouldNotSatisfy` rangesOverlap (Range 7 10)
        Range 7 10 `shouldNotSatisfy` rangesOverlap (Range 3 3)

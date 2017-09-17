module Checkmate.DiffSpec (spec) where

import Data.Range.Range as Range
import Test.Hspec
import Test.QuickCheck
import Text.Diff.Parse.Types as DT

import Checkmate.Diff (rangeFromDiffRange)

instance Arbitrary DT.Range where
    arbitrary = do
        NonZero start <- arbitrary
        NonZero lineLen <- arbitrary
        return $ DT.Range (abs start) (abs lineLen)

spec :: Spec
spec =
    specify "rangeFromDiffRange" $ property $ \ dr@(DT.Range start lineLen) ->
        let r = rangeFromDiffRange dr
        in
            Range.inRange r start &&
            not (Range.inRange r $ start - 1) &&
            Range.inRange r (start + lineLen - 1) &&
            not (Range.inRange r $ start + lineLen)

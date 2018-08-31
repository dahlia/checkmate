module Checkmate.Range
    ( Range (..)
    , rangesOverlap
    ) where

import Text.Diff.Parse.Types

rangesOverlap :: Range -> Range -> Bool
rangesOverlap (Range aFrom aLen) (Range bFrom bLen) =
    aFrom <= bFrom && bFrom < aFrom + aLen ||
        bFrom <= aFrom && aFrom < bFrom + bLen

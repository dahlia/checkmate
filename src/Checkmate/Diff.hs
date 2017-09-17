module Checkmate.Diff
    ( rangeFromDiffRange
    ) where

import Data.Range.Range as Range
import Text.Diff.Parse.Types as DT

rangeFromDiffRange :: DT.Range -> Range.Range Int
rangeFromDiffRange (DT.Range start lineLen)
  | lineLen < 2 = Range.SingletonRange start
  | otherwise = Range.SpanRange start $ start + (lineLen - 1)

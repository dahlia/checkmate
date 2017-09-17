module Checkmate.Check
    ( Check (Check, checkOrderIndex, checkScope, checkText)
    , Checklist
    , Scope (Directory, FileBlock, scopePath, scopeRange)
    , toList
    , union
    ) where

import Data.Set hiding (toList)
import Data.Text
import Data.Range.Range hiding (union)
import System.FilePath

data Scope
    = FileBlock { scopePath :: FilePath, scopeRange :: Range Int }
    | Directory { scopePath :: FilePath }
    deriving (Show)

instance Eq Scope where
    FileBlock pathA rangeA == FileBlock pathB rangeB =
        equalFilePath pathA pathB && rangeA == rangeB
    Directory pathA == Directory pathB =
        equalFilePath pathA pathB
    _ == _ = False

data Check
    = Check { checkScope :: Scope
            , checkOrderIndex :: Int
            , checkText :: Text
            } deriving (Eq, Show)

instance Ord Check where
    compare a b =
        compare (tup a) (tup b)
      where
        tup (Check scope orderIndex text) = (scopePath scope, orderIndex, text)

type Checklist = Set Check

toList :: Checklist -> [Check]
toList = toAscList

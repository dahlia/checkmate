{-# LANGUAGE OverloadedStrings #-}
module Checkmate.Renderer
    ( toCommonMark
    , toGFMarkdown
    ) where

import Data.List

import Data.Set
import Data.Text hiding (groupBy, isSuffixOf, zip)
import System.FilePath

import Checkmate.Check

toCommonMark :: FilePath -> Int -> Checklist -> Text
toCommonMark = toCommonMark' ""

toGFMarkdown :: FilePath -> Int -> Checklist -> Text
toGFMarkdown = toCommonMark' "[ ] "

toCommonMark' :: Text -> FilePath -> Int -> Checklist -> Text
toCommonMark' itemPrefix basePath headingLevel checklist = cat $
    [heading, " Checklist  \x1f914\n"] ++
    [ cat $
        (if i == (1 :: Int)
            then ["\n", heading, "# `", titlePath s, "`\n\n"]
            else []
            ) ++
          [ " -  "
          , itemPrefix
          , replace "\n" "\n    " t
          , "\n"
          ]
    | fileChecklist <- checks
    , (i, Check { checkScope = s, checkText = t }) <- zip [1..] fileChecklist
    ]
  where
    checks :: [[Check]]
    checks = groupBy scopePathEquals $ toAscList checklist
    scopePathEquals :: Check -> Check -> Bool
    scopePathEquals Check { checkScope = a } Check { checkScope = b } =
        scopePath a `equalFilePath` scopePath b
    cat :: [Text] -> Text
    cat = Data.Text.concat
    heading :: Text
    heading = pack ['#' | _ <- [1..headingLevel]]
    titlePath :: Scope -> Text
    titlePath scope = Data.Text.map
        (\ c -> if c == pathSeparator then '/' else c) $
        pack $ makeRelative basePath $ normalise p
      where
        p :: FilePath
        p = case scope of
                Directory p' -> addTrailingPathSeparator p'
                FileBlock { scopePath = p' } -> p'

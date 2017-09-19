{-# LANGUAGE OverloadedStrings #-}
module Checkmate.Renderer
    ( toCommonMark
    , toGFMarkdown
    ) where

import Data.Foldable

import Data.Text

import Checkmate.Check

toCommonMark :: Checklist -> Text
toCommonMark = toCommonMark' ""

toGFMarkdown :: Checklist -> Text
toGFMarkdown = toCommonMark' "[ ] "

toCommonMark' :: Text -> Checklist -> Text
toCommonMark' itemPrefix checklist =
    "### Checklist  \x1f914\n\n" `append` cat
        [ cat [ " -  "
              , itemPrefix
              , replace "\n" "\n    " t
              , "\n"
              ]
        | Check { checkText = t } <- toList checklist
        ]
  where
    cat :: [Text] -> Text
    cat = Data.Text.concat

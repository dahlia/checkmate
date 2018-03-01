{-# LANGUAGE OverloadedStrings #-}
module Checkmate.Parser.IndentBlock
    ( ParseError
    , parser
    , parseSourceCode
    , parseSourceFile
    ) where

import Control.Monad
import Data.List
import Data.Void

import Data.Range.Range
import Data.Set
import Data.Text
import Data.Text.IO
import Text.Megaparsec hiding (ParseError)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Error as E

import Checkmate.Check

type Parser = Parsec Void Text
type ParseError = E.ParseError Char Void

parseSourceFile :: FilePath -> IO (Either ParseError Checklist)
parseSourceFile filePath = do
    input <- Data.Text.IO.readFile filePath
    return $ parseSourceCode filePath input

parseSourceCode :: FilePath -> Text -> Either ParseError Checklist
parseSourceCode = parse parser 

data Line = CheckComment Text | Line | EmptyLine deriving (Eq, Ord, Show)

parser :: Parser Checklist
parser = do
    lines' <- line `sepEndBy` eol
    eof
    let checkData = analyzeIndents 0 [] lines'
        sorted = sort checkData
        checks = [ Check (FileBlock path $ mkRange fromTo) i text
                 | (i, (path, fromTo, _, text)) <- Data.List.zip [1..] sorted
                 ]
    return $ Data.Set.fromList checks
  where
    mkRange :: (Int, Int) -> Range Int
    mkRange (from, to)
     | from >= to = SingletonRange from
     | otherwise = SpanRange from to
    indent :: Parser Int
    indent = choice
        [ tab >> return 8
        , char ' ' >> return 1
        ]
    someSpaces :: Parser ()
    someSpaces = skipMany $ oneOf [' ', '\t']
    checkKeyword :: Parser ()
    checkKeyword = void $ string "CHECK"
    checkThenSpaces :: Parser ()
    checkThenSpaces = do
        checkKeyword
        (char ':' >> someSpaces) <|> skipSome (oneOf [' ', '\t'])
    lineCommentStart :: Parser Text
    lineCommentStart = choice
        [ Data.Text.singleton <$> oneOf ['#', '%']
        , string "//"
        , string "--"
        ]
    lineCommentCheck :: Parser Text
    lineCommentCheck = do
        startSeq <- lineCommentStart
        someSpaces
        checkThenSpaces
        chars <- many $ noneOf ['\n']
        nextLines <- many $ try $ do
            void eol
            someSpaces
            void $ string startSeq
            someSpaces
            many $ noneOf ['\n']
        return $ stripEnd $ pack $ Data.List.unlines $ chars : nextLines
    blockCommentPairs :: [(Text, Text)]
    blockCommentPairs =
        [ ("/*", "*/"), ("{-", "-}"), ("<!--", "-->"), ("<#", "#>")
        , ("%{", "%}")
        ]
    blockCommentCheck :: Int -> Parser Text
    blockCommentCheck depth =
        choice $ fmap (blockComment depth) blockCommentPairs
    blockComment :: Int -> (Text, Text) -> Parser Text
    blockComment depth (start, end) = do
        void $ string start
        linebreaks <- many $ try $ do
            skipMany $ oneOf [' ', '\t', '\r']
            char '\n'
        innerDepth <- many indent
        checkThenSpaces
        chars <- manyTill anyChar (string end)
        let leftPadding = case linebreaks of
                [] -> depth
                _ -> sum innerDepth
        return $ stripEnd $ pack $ stripLeftPadding leftPadding chars
    line :: Parser (FilePath, Int, Int, Line)
    line = do
        SourcePos filePath lineNo _ <- getPosition
        widths <- many indent
        let depth = sum widths
        lineT <- choice
            [ try $ fmap CheckComment lineCommentCheck
            , try $ CheckComment <$> blockCommentCheck depth
            , try (some (noneOf ['\n']) >> return Line)
            , return EmptyLine
            ]
        return (filePath, read . show . unPos $ lineNo, depth, lineT)
    stripLeftPadding :: Int -> String -> String
    stripLeftPadding width =
        Data.List.unlines . fmap (lstrip width) . Data.List.lines
      where
        lstrip :: Int -> String -> String
        lstrip _ [] = []
        lstrip 0 txt = txt
        lstrip w (' ' : xs) = lstrip (w - 1) xs
        lstrip w txt@('\t' : xs) = if w >= 8 then lstrip (w - 8) xs else txt
        lstrip _ txt = txt
    analyzeIndents :: Int
                   -> [(FilePath, (Int, Int), Int, Text)]
                   -> [(FilePath, Int, Int, Line)]
                   -> [(FilePath, (Int, Int), Int, Text)]
    analyzeIndents  _ prev [] = prev
    analyzeIndents prevDepth prev ((_, _, _, EmptyLine) : rest) =
        -- Indent blocks usually continue through empty lines
        -- (i.e. /^[ \t]{0}$/)
        analyzeIndents prevDepth prev rest
    analyzeIndents prevDepth prev ((filePath, lineNo, depth, lineT) : rest) =
        dedented ++ analyzeIndents depth next rest
      where
        isDedented :: (FilePath, (Int, Int), Int, Text) -> Bool
        isDedented (_, _, checkDepth, _) =
            prevDepth > depth && checkDepth > depth
        (dedented, inScope) = Data.List.partition isDedented prev
        next :: [(FilePath, (Int, Int), Int, Text)]
        next =
            [ (path, (from, lineNo), d, t)
            | (path, (from, _), d, t) <- inScope
            ] ++ case lineT of
                     CheckComment t -> [(filePath, (lineNo, lineNo), depth, t)]
                     _ -> []

module Checkmate.Parser.IndentBlock
    ( parser
    , parseSourceCode
    , parseSourceFile
    ) where

import Control.Monad
import Data.List

import Data.Range.Range
import Data.Set
import Data.Text
import Data.Text.IO
import Text.Megaparsec
import Text.Megaparsec.Text

import Checkmate.Check

parseSourceFile :: FilePath
                -> IO (Either (ParseError (Token Text) Dec) Checklist)
parseSourceFile filePath = do
    input <- Data.Text.IO.readFile filePath
    return $ parseSourceCode filePath input

parseSourceCode :: FilePath
                -> Text
                -> Either (ParseError (Token Text) Dec) Checklist
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
    someSpaces = skipMany $ oneOf " \t"
    checkKeyword :: Parser ()
    checkKeyword = do
        someSpaces
        void $ string "CHECK"
        someSpaces
    lineCommentCheck :: Parser Text
    lineCommentCheck = do
        choice [void $ oneOf "#%'", void $ string "//", void $ string "--"]
        checkKeyword
        chars <- many $ noneOf "\n"
        return $ pack chars
    blockCommentPairs :: [(String, String)]
    blockCommentPairs =
        [ ("/*", "*/"), ("{-", "-}"), ("<!--", "-->"), ("<#", "#>")
        , ("%{", "%}")
        ]
    blockCommentCheck :: Parser Text
    blockCommentCheck = choice $ fmap blockComment blockCommentPairs
    blockComment :: (String, String) -> Parser Text
    blockComment (start, end) = do
        void $ string start
        checkKeyword
        chars <- manyTill anyChar (string end)
        return $ pack chars
    line :: Parser (FilePath, Int, Int, Line)
    line = do
        SourcePos filePath lineNo _ <- getPosition
        widths <- many indent
        lineT <- choice
            [ try $ fmap CheckComment lineCommentCheck
            , try $ fmap CheckComment blockCommentCheck
            , try (some (noneOf "\n") >> return Line)
            , return EmptyLine
            ]
        return (filePath, read . show . unPos $ lineNo, sum widths, lineT)
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

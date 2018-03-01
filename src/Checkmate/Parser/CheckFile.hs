{-# LANGUAGE OverloadedStrings #-}
module Checkmate.Parser.CheckFile
    ( ParseError
    , parseCheckFile
    , parseCheckFileText
    , parser
    ) where

import Control.Monad
import Data.Void

import Data.Set
import qualified Data.Text as T
import Data.Text.IO
import System.FilePath
import Text.Megaparsec hiding (ParseError)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Error as E

import Checkmate.Check

type Parser = Parsec Void T.Text
type ParseError = E.ParseError Char Void

parseCheckFile :: FilePath -> IO (Either ParseError Checklist)
parseCheckFile filePath = do
    input <- Data.Text.IO.readFile filePath
    return $ parseCheckFileText filePath input

parseCheckFileText :: FilePath -> T.Text -> Either ParseError Checklist
parseCheckFileText filePath =
    parse (parser $ takeDirectory filePath) filePath

parser :: FilePath -> Parser Checklist
parser dirPath = do
    space
    checkTexts <- many checkP
    eof
    return $ Data.Set.fromList
        [Check (Directory dirPath) i ct | (i, ct) <- zip [1..] checkTexts]
  where
    bulletP :: Parser ()
    bulletP = do
        skipMany (spaceChar <|> tab)
        bullet <- choice [ string "*"
                         , string "+"
                         , string "-"
                         , string "CHECK:"
                         , string "CHECK"
                         , some digitChar >> (string "." <|> string ")")
                         ]
        skipSome (spaceChar <|> tab)
        unless (bullet == "CHECK") $ void . optional $ do
            void $ string "CHECK"
            skipSome (spaceChar <|> tab)
    checkP :: Parser T.Text
    checkP = do
        bulletP
        texts <- (`sepBy1` try (eol >> notFollowedBy bulletP)) $ do
            chars <- many $ noneOf ['\n']
            return $ T.pack chars
        let texts' = texts :: [T.Text]
        return $ T.strip $ T.intercalate (T.singleton '\n') texts'

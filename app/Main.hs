{-# LANGUAGE OverloadedStrings #-}
import Data.Semigroup ((<>))
import System.IO

import Data.Text
import Data.Text.IO as TIO
import Options.Applicative
import System.Directory

import Checkmate.Check
import Checkmate.Discover

data App = App
    { inputFilePath :: FilePath
    } deriving (Show)

withInputFile :: App -> (Handle -> IO r) -> IO r
withInputFile App { inputFilePath = "-" } action' = do
    let i = stdin
    r <- action' i
    hClose i
    return r
withInputFile App { inputFilePath = i } action' =
    withFile i ReadMode action'

appP :: Parser App
appP = App
    <$> strOption (  long "input-file"
                  <> short 'i'
                  <> metavar "FILE"
                  <> showDefault
                  <> value "-"
                  <> help "perform"
                  )

appPI :: ParserInfo App
appPI = info (appP <**> helper)
    (  fullDesc
    <> progDesc "Generate checklists relevant to a given patch."
    )

toGFMarkdown :: Checklist -> Text
toGFMarkdown checklist =
    intercalate
        "\n"
        [ " -  [ ] " `append` replace "\n" "\n    " t
        | Check { checkText = t } <- toList checklist
        ]

main :: IO ()
main = do
    app <- execParser appPI
    cwd <- getCurrentDirectory
    diff <- withInputFile app TIO.hGetContents
    case parseDiff diff of
        Left msg -> System.IO.putStr msg
        Right deltas -> do
            checklist <- discover cwd deltas
            TIO.putStrLn $ toGFMarkdown checklist

{-# LANGUAGE OverloadedStrings #-}
import Data.Semigroup ((<>))
import System.IO

import Data.Text
import Data.Text.IO as TIO
import Options.Applicative
import System.Directory

import Checkmate.Check
import Checkmate.Discover

type Command = App -> Checklist -> IO ()

data App = App
    { inputFilePath :: FilePath
    , appCommand :: Command
    }

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
    <*> subparser (  command "commonmark" commonMarkPI
                  <> command "gfm" gfmPI
                  )

commonMarkPI :: ParserInfo Command
commonMarkPI = info (pure cmd) $
    progDesc "Print a checklist as CommonMark (i.e. Markdown) format."
  where
    cmd :: Command
    cmd _ = TIO.putStr . (`toCommonMark` "")

gfmPI :: ParserInfo Command
gfmPI = info (pure cmd) $
    progDesc "Print a checklist as GitHub Flavored Markdown format."
  where
    cmd :: Command
    cmd _ = TIO.putStr . toGFMarkdown

appPI :: ParserInfo App
appPI = info (appP <**> helper)
    (  fullDesc
    <> progDesc "Generate checklists relevant to a given patch."
    )

toGFMarkdown :: Checklist -> Text
toGFMarkdown = (`toCommonMark` "[ ] ")

toCommonMark :: Checklist -> Text -> Text
toCommonMark checklist prefix =
    intercalate
        "\n"
        [ " -  " `append` prefix `append` replace "\n" "\n    " t
        | Check { checkText = t } <- toList checklist
        ]

main :: IO ()
main = do
    app@App { appCommand = cmd' } <- execParser appPI
    cwd <- getCurrentDirectory
    diff <- withInputFile app TIO.hGetContents
    case parseDiff diff of
        Left msg -> System.IO.putStrLn msg
        Right deltas -> do
            checklist <- discover cwd deltas
            cmd' app checklist

{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Data.Maybe
import Data.Semigroup ((<>))
import Prelude hiding (error)
import System.Environment
import System.Exit
import System.IO

import Data.Text
import Data.Text.Encoding
import Data.Text.IO as TIO
import Options.Applicative
import System.Directory

import Checkmate.Check
import Checkmate.Discover
import Checkmate.Publisher.GitHub
import Checkmate.Renderer

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
                  <> help "A diff text to extract a checklist from"
                  )
    <*> subparser (  command "commonmark" commonmarkPI
                  <> command "gfm" gfmPI
                  <> command "github" githubPI
                  <> command "github-travis" githubTravisPI
                  )

commonmarkPI :: ParserInfo Command
commonmarkPI = info (pure cmd) $
    progDesc "Print a checklist as CommonMark (i.e. Markdown) format."
  where
    cmd :: Command
    cmd _ checklist = do
        cwd <- getCurrentDirectory
        TIO.putStr $ toCommonMark cwd 1 checklist

gfmPI :: ParserInfo Command
gfmPI = info (pure cmd) $
    progDesc "Print a checklist as GitHub Flavored Markdown format."
  where
    cmd :: Command
    cmd _ checklist = do
        cwd <- getCurrentDirectory
        TIO.putStr $ toGFMarkdown cwd 1 checklist

githubTokenOption :: Parser Token
githubTokenOption = option (encodeUtf8 . pack <$> str)
    (  long "access-token"
    <> long "token"
    <> short 't'
    <> metavar "TOKEN"
    <> help "GitHub OAuth access token"
    )

leaveGithubComment :: Maybe OwnerName
                   -> RepoName
                   -> IssueId
                   -> Token
                   -> Maybe Text
                   -> Command
leaveGithubComment owner' repo pr accessToken endpoint _ checklist = do
    cwd <- getCurrentDirectory
    r <- leaveComment owner' repo pr accessToken endpoint cwd checklist
    case r of
        Right Nothing -> return ()
        Right (Just (URL url)) -> TIO.putStrLn url
        Left (HTTPError httpError) -> printError $ pack $ show httpError
        Left (ParseError message) -> printError message
        Left (JsonError message) -> printError message
        Left (UserError message) -> printError message

githubPI :: ParserInfo Command
githubPI = info (parser <**> helper) $
    progDesc $ "Create a checklist comment on the corresponding pull " ++
               "reuqest on GitHub."
  where
    parser :: Parser Command
    parser = leaveGithubComment
        <$> option (Just . mkOwnerName . pack <$> str)
            (  long "owner"
            <> long "login"
            <> short 'l'
            <> metavar "LOGIN"
            <> value Nothing
            <> help ("Owner of GitHub repository of a pull request to " ++
                     "create a checklist comment.  \"foo\" of " ++
                     "\"github.com/foo/bar\".  The currently authenticated " ++
                     "user (through -t/--access-token/--token) by default")
            )
        <*> option (mkRepoName . pack <$> str)
            (  long "repository"
            <> long "repo"
            <> short 'r'
            <> metavar "REPO"
            <> help ("Name of GitHub repository of a pull request to create " ++
                     "a checklist comment.  \"bar\" of \"github.com/foo/bar\"")
            )
        <*> option (mkIssueId <$> (auto :: ReadM Int))
            (  long "pull-request"
            <> long "pr"
            <> short 'p'
            <> metavar "NUM"
            <> help "No. of pull request to create a checklist comment"
            )
        <*> githubTokenOption
        <*> option (Just . dropWhileEnd (== '/') . pack <$> str)
            (  long "enterprise-endpoint"
            <> short 'e'
            <> metavar "URL"
            <> value Nothing
            <> help "API endpoint URL for GitHub Enterprise (if applicable)"
            )

githubTravisPI :: ParserInfo Command
githubTravisPI = info (parser <**> helper) $
    progDesc $ "Create a checklist comment on the corresponding pull " ++
               "reuqest on GitHub from Travis CI. It depends on the " ++
               "following environment variables: TRAVIS_REPO_SLUG, " ++
               "TRAVIS_PULL_REQUEST."
  where
    parser :: Parser Command
    parser = cmd <$> githubTokenOption
    cmd :: Token -> Command
    cmd accessToken app checklist = do
        pr <- environ "TRAVIS_PULL_REQUEST"
        when (pr == "false") $ printError "This is not a PR build; skipped..."
        let prNo = mkIssueId $ read pr
        slug <- pack <$> environ "TRAVIS_REPO_SLUG"
        let (o, r) = Data.Text.break (== '/') slug
            owner = mkOwnerName o
            repo = mkRepoName $ Data.Text.drop 1 r
        leaveGithubComment
            (Just owner)
            repo
            prNo
            accessToken
            Nothing
            app
            checklist
    environ :: String -> IO String
    environ name = do
        r <- lookupEnv name
        case r of
            Nothing -> printError $ pack name `append` " is not defined."
            Just v -> return v

appPI :: ParserInfo App
appPI = info (appP <**> helper)
    (  fullDesc
    <> progDesc "Generate checklists relevant to a given patch."
    )

printError :: Text -> IO a
printError message = do
    prog <- getProgName
    die $ prog ++ ": error: " ++ unpack message

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

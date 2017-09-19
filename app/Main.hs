{-# LANGUAGE OverloadedStrings #-}
import Data.Foldable
import Data.Maybe
import Data.Semigroup ((<>))
import Prelude hiding (error)
import System.Environment
import System.Exit
import System.IO

import Data.Text hiding (find)
import Data.Text.Encoding
import Data.Text.IO as TIO
import GitHub.Auth
import GitHub.Data.Comments
import GitHub.Data.Definitions
import GitHub.Data.Id
import GitHub.Data.Issues
import GitHub.Data.Name
import GitHub.Data.Repos
import GitHub.Data.URL
import GitHub.Endpoints.Issues.Comments (comments', createComment, editComment)
import GitHub.Endpoints.Users (userInfoCurrent')
import Options.Applicative
import System.Directory

import Checkmate.Check
import Checkmate.Discover
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
                  )

commonmarkPI :: ParserInfo Command
commonmarkPI = info (pure cmd) $
    progDesc "Print a checklist as CommonMark (i.e. Markdown) format."
  where
    cmd :: Command
    cmd _ = TIO.putStr . toCommonMark

gfmPI :: ParserInfo Command
gfmPI = info (pure cmd) $
    progDesc "Print a checklist as GitHub Flavored Markdown format."
  where
    cmd :: Command
    cmd _ = TIO.putStr . toGFMarkdown

githubPI :: ParserInfo Command
githubPI = info (parser <**> helper) $
    progDesc $ "Create a checklist comment on the corresponding pull " ++
               "reuqest on GitHub."
  where
    cmd :: Maybe (Name Owner)
        -> Name Repo
        -> Id Issue
        -> Token
        -> Maybe Text
        -> Command
    cmd owner' repo pr accessToken endpoint _ checklist = do
        user <- userInfoCurrent' auth >>= error
        let owner = fromMaybe (N . untagName $ userLogin user) owner'
        prComments <- comments' (Just auth) owner repo pr >>= error
        let checklistComment = find (isChecklist user) prComments
            leave = case checklistComment of
                Nothing -> createComment auth owner repo pr
                Just IssueComment { issueCommentId = cid } ->
                    editComment auth owner repo $ Id cid
        Comment { commentHtmlUrl = leftCommentUrl } <-
            leave (signature `append` toGFMarkdown checklist) >>= error
        case leftCommentUrl of
            Just (URL u) -> TIO.putStrLn u
            _ -> return ()
      where
        auth :: Auth
        auth = case endpoint of
            Nothing -> OAuth accessToken
            Just e -> EnterpriseOAuth e accessToken
        signature :: Text
        signature = "<!-- COMMENT BY CHECKMATE -->\n"
        isChecklist :: User -> IssueComment -> Bool
        isChecklist User { userId = uid }
                    IssueComment { issueCommentBody = cBody
                                 , issueCommentUser =
                                       SimpleUser { simpleUserId = authorId }
                                 } =
            uid == authorId && isPrefixOf signature cBody
    error :: Either Error a -> IO a
    error (Right v) = return v
    error (Left (HTTPError httpError)) = printError $ pack $ show httpError
    error (Left (ParseError message)) = printError message
    error (Left (JsonError message)) = printError message
    error (Left (UserError message)) = printError message
    parser :: Parser Command
    parser = cmd
        <$> option (Just . (N :: Text -> Name Owner) . pack <$> str)
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
        <*> option ((N :: Text -> Name Repo) . pack <$> str)
            (  long "repository"
            <> long "repo"
            <> short 'r'
            <> metavar "REPO"
            <> help ("Name of GitHub repository of a pull request to create " ++
                     "a checklist comment.  \"bar\" of \"github.com/foo/bar\"")
            )
        <*> option ((Id :: Int -> Id Issue) <$> (auto :: ReadM Int))
            (  long "pull-request"
            <> long "pr"
            <> short 'p'
            <> metavar "NUM"
            <> help "No. of pull request to create a checklist comment"
            )
        <*> option (encodeUtf8 . pack <$> str)
            (  long "access-token"
            <> long "token"
            <> short 't'
            <> metavar "TOKEN"
            <> help "GitHub OAuth access token"
            )
        <*> option (Just . dropWhileEnd (== '/') . pack <$> str)
            (  long "enterprise-endpoint"
            <> short 'e'
            <> metavar "URL"
            <> value Nothing
            <> help "API endpoint URL for GitHub Enterprise (if applicable)"
            )

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

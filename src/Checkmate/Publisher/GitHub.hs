{-# LANGUAGE OverloadedStrings #-}
module Checkmate.Publisher.GitHub
    ( Error (..)
    , IssueId
    , OwnerName
    , RepoName
    , Token
    , URL (URL)
    , leaveComment
    , mkIssueId
    , mkOwnerName
    , mkRepoName
    ) where

import Control.Exception
import Data.Foldable
import Data.Maybe

import Data.Text hiding (find, null)
import GitHub.Auth
import GitHub.Data.Comments
import GitHub.Data.Definitions
import GitHub.Data.Id
import GitHub.Data.Issues
import GitHub.Data.Name
import GitHub.Data.Repos
import GitHub.Data.URL
import GitHub.Endpoints.Issues.Comments
    ( comments'
    , createComment
    , deleteComment
    , editComment
    )
import GitHub.Endpoints.Users (userInfoCurrent')

import Checkmate.Check (Checklist)
import Checkmate.Renderer

type IssueId = Id Issue
type OwnerName = Name Owner
type RepoName = Name Repo

mkIssueId :: Int -> IssueId
mkIssueId = Id

mkOwnerName :: Text -> OwnerName
mkOwnerName = N

mkRepoName :: Text -> RepoName
mkRepoName = N

leaveComment :: Maybe OwnerName
             -> RepoName
             -> IssueId
             -> Token
             -> Maybe Text
             -> FilePath
             -> Checklist
             -> IO (Either Error (Maybe URL))
leaveComment owner' repo pr accessToken endpoint basePath checklist = try $ do
    user <- userInfoCurrent' auth >>= error'
    let owner = fromMaybe (N . untagName $ userLogin user) owner'
    prComments <- comments' (Just auth) owner repo pr >>= error'
    let checklistComment = find (isChecklist user) prComments
    if null checklist
        then
            case checklistComment of
                    Nothing -> return Nothing
                    Just IssueComment { issueCommentId = cid } -> do
                        deleteComment auth owner repo (Id cid) >>= error'
                        return Nothing
        else do
            let leave = case checklistComment of
                    Nothing -> createComment auth owner repo pr
                    Just IssueComment { issueCommentId = cid } ->
                        editComment auth owner repo $ Id cid
            Comment { commentHtmlUrl = leftCommentUrl } <-
                leave (signature `append` toGFMarkdown basePath 3 checklist)
                    >>= error'
            return leftCommentUrl
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
    error' :: Either Error a -> IO a
    error' (Right v) = return v
    error' (Left e) = throwIO e

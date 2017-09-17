module Checkmate.Discover
    ( discover
    , discoverDirectory
    , discoverFile
    , parseDiff
    ) where

import Control.Monad.Parallel as P
import Data.Range.Range as Range
import Data.Set as S
import Data.Text
import System.Directory
import System.FilePath
import Text.Diff.Parse (parseDiff)
import Text.Diff.Parse.Types

import Checkmate.Check hiding (toList)
import Checkmate.Diff
import Checkmate.Parser.CheckFile
import Checkmate.Parser.IndentBlock

checkFileName :: FilePath
checkFileName = "CHECK"

discover :: FilePath -> FileDeltas -> IO Checklist
discover baseDirPath deltas = do
    checklists <- P.sequence $ fmap (discoverFile baseDirPath) deltas
    dirChecklists <- forM deltas $ \ FileDelta { fileDeltaDestFile = p } -> do
        let dirPath' = takeDirectory $ unpack p
        discoverDirectory baseDirPath dirPath'
    return . unions $ checklists ++ dirChecklists

discoverDirectory :: FilePath -> FilePath -> IO Checklist
discoverDirectory baseDirPath dirPath = do
    dirExist <- doesDirectoryExist dirPath'
    if not dirExist then return S.empty else do
        checkFileExist <- doesFileExist checkFilePath
        checklist <-
            if checkFileExist
            then do
                result <- parseCheckFile checkFilePath
                return $ case result of
                    Left _ -> S.empty
                    Right checklist -> checklist
            else return S.empty
        parentChecklist <-
            if dirPath == "." || dirPath == "" || parent == dirPath
            then return S.empty
            else discoverDirectory baseDirPath parent
        return $ S.union checklist parentChecklist
  where
    dirPath' :: FilePath
    dirPath' = baseDirPath </> dirPath
    checkFilePath :: FilePath
    checkFilePath = dirPath' </> checkFileName
    parent :: FilePath
    parent = takeDirectory dirPath

discoverFile :: FilePath -> FileDelta -> IO Checklist
discoverFile _ FileDelta { fileDeltaContent = Binary } = return S.empty
discoverFile baseDirPath FileDelta { fileDeltaContent = Hunks hunks
                                   , fileDeltaDestFile = filePathT
                                   } = do
    result <- parseSourceFile filePath
    return $ case result of
        Left _ -> S.empty
        Right checklist -> fromList
            [ c
            | c@Check { checkScope = FileBlock _ range } <- toList checklist
            , Prelude.any (rangesOverlap range) ranges
            ]
  where
    filePath :: FilePath
    filePath = baseDirPath </> unpack filePathT
    ranges :: [Range.Range Int]
    ranges = [rangeFromDiffRange r | Hunk { hunkDestRange = r } <- hunks]

module Checkmate.Discover
    ( FileDeltas
    , discover
    , discoverDirectory
    , discoverFile
    , parseDiff
    ) where

import System.IO.Error

import Control.Monad.Parallel as P
import Data.Set as S
import Data.Text
import System.Directory
import System.FilePath
import Text.Diff.Parse (parseDiff)
import Text.Diff.Parse.Types

import Checkmate.Check
import Checkmate.Parser.CheckFile
import Checkmate.Parser.IndentBlock
import Checkmate.Range

checkFileNames :: Set FilePath
checkFileNames = fromList
    -- CHECK: All checklist filenames that Checkmate recognize should be
    -- written in the project docs (i.e. README.md).
    [ "CHECK"
    , ".check"
    ]

discover :: FilePath -> FileDeltas -> IO Checklist
discover baseDirPath deltas = do
    checklists <- P.sequence $ fmap (discoverFile baseDirPath) deltas
    dirChecklists <- forM deltas $ \ FileDelta { fileDeltaDestFile = p } -> do
        let dirPath' = takeDirectory $ unpack p
        discoverDirectory baseDirPath dirPath'
    return . unions $ checklists ++ dirChecklists

discoverDirectory :: FilePath -> FilePath -> IO Checklist
discoverDirectory baseDirPath dirPath = fmap unions $ forM checkFileNames' $
    \ checkFileName -> do
        let checkFilePath = dirPath' </> checkFileName
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
    checkFileNames' :: [FilePath]
    checkFileNames' = toList checkFileNames
    dirPath' :: FilePath
    dirPath' = baseDirPath </> dirPath
    parent :: FilePath
    parent = takeDirectory dirPath

discoverFile :: FilePath -> FileDelta -> IO Checklist
discoverFile _ FileDelta { fileDeltaContent = Binary } = return S.empty
discoverFile baseDirPath FileDelta { fileDeltaContent = Hunks hunks
                                   , fileDeltaDestFile = filePathT
                                   } = do
    result <- catchIOError (parseSourceFile filePath) $ \ e ->
        if isDoesNotExistError e
           then return $ Right S.empty
           else ioError e
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
    ranges :: [Range]
    ranges = [r | Hunk { hunkDestRange = r } <- hunks]

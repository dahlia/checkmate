{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Checkmate.DiscoverSpec (spec) where

import Control.Monad

import Data.ByteString
import Data.FileEmbed
import Data.Range.Range (Range (..))
import Data.Text
import System.Directory
import System.FilePath
import System.IO.Temp
import Test.Hspec
import Text.Diff.Parse (parseDiff)
import Text.Diff.Parse.Types

import Checkmate.Check
import Checkmate.Discover

fixtures :: [(FilePath, ByteString)]
fixtures = $(embedDir "test/fixtures/")

diffTextFixture :: Text
diffTextFixture = $(embedStringFile "test/fixtures/patch.diff")

diffFixture :: FileDeltas
Right diffFixture = parseDiff diffTextFixture

rootChecklistFixture :: FilePath -> Checklist
rootChecklistFixture dirPath =
    [ Check { checkScope = dir
            , checkOrderIndex = 1
            , checkText = "Sample check A"
            }
    , Check { checkScope = dir
            , checkOrderIndex = 2
            , checkText = "sample check B"
            }
    , Check { checkScope = dir
            , checkOrderIndex = 3
            , checkText = "Sample check C"
            }
    ]
  where
    dir :: Scope
    dir = Directory dirPath

subdirChecklistFixture :: FilePath -> Checklist
subdirChecklistFixture dirPath =
    [ Check { checkScope = dir
            , checkOrderIndex = 1
            , checkText = "sub-dir check A"
            }
    , Check { checkScope = dir
            , checkOrderIndex = 2
            , checkText = "sub-dir check B"
            }
    ] `union` rootChecklistFixture dirPath
  where
    dir :: Scope
    dir = Directory $ dirPath </> "subdir"

pyChecklistFixture :: FilePath -> Checklist
pyChecklistFixture d =
    [ Check { checkScope = FileBlock { scopePath = pyPath
                                     , scopeRange = SpanRange 2 23
                                     }
            , checkOrderIndex = 1
            , checkText = "module-level check"
            }
    , Check { checkScope = FileBlock { scopePath = pyPath
                                     , scopeRange = SpanRange 11 15
                                     }
            , checkOrderIndex = 3
            , checkText = "function-level check 2"
            }
    , Check { checkScope = FileBlock { scopePath = pyPath
                                     , scopeRange = SpanRange 14 15
                                     }
            , checkOrderIndex = 4
            , checkText = "nested function-level check"
            }
    ]
  where
    pyPath :: FilePath
    pyPath = d </> "sample.py"

jsChecklistFixture :: FilePath -> Checklist
jsChecklistFixture d =
    [ Check { checkScope = FileBlock { scopePath = jsPath
                                     , scopeRange = SpanRange 2 18
                                     }
            , checkOrderIndex = 1
            , checkText = "global check"
            }
    , Check { checkScope = FileBlock { scopePath = jsPath
                                     , scopeRange = SpanRange 10 17
                                     }
            , checkOrderIndex = 3
            , checkText = "function-level check 2"
            }
    , Check { checkScope = FileBlock { scopePath = jsPath
                                     , scopeRange = SpanRange 13 14
                                     }
            , checkOrderIndex = 4
            , checkText = "closure check"
            }
    ]
  where
    jsPath :: FilePath
    jsPath = d </> "subdir" </> "inner_sample.js"

withFixtureDir :: (FilePath -> IO a) -> IO a
withFixtureDir action =
    withSystemTempDirectory "checkmate-test" $ \ dirPath -> do
        forM_ fixtures $ \ (filePath, contents) -> do
            let path = dirPath </> filePath
                parent = takeDirectory path
                -- `parent` can differ from `dirPath` when `filePath` has '/'
            createDirectoryIfMissing True parent
            Data.ByteString.writeFile path contents
        action dirPath

spec :: Spec
spec = do
    specify "discover" $
        withFixtureDir $ \ dirPath -> do
            checklist <- discover dirPath diffFixture
            let expected = subdirChecklistFixture dirPath
                    `union` pyChecklistFixture dirPath
                    `union` jsChecklistFixture dirPath
            checklist `shouldBe` expected
    specify "discoverDirectory" $
        withFixtureDir $ \ dirPath -> do
            subdirChecks <- discoverDirectory dirPath "subdir"
            subdirChecks `shouldBe` subdirChecklistFixture dirPath
            rootChecks <- discoverDirectory dirPath "."
            rootChecks `shouldBe` rootChecklistFixture dirPath
    specify "discoverFile" $
        withFixtureDir $ \ dirPath -> do
            pyChecks <- discoverFile dirPath $ Prelude.head diffFixture
            pyChecks `shouldBe` pyChecklistFixture dirPath
            jsChecks <- discoverFile dirPath $ Prelude.last diffFixture
            jsChecks `shouldBe` jsChecklistFixture dirPath

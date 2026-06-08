{-# LANGUAGE OverloadedStrings #-}

module PathTrieSpec (spec) where

import Data.List (isInfixOf)
import Data.Text (Text)

import Hgs.Domain
  ( Package(..)
  , PackageName(..)
  , PackageSource(..)
  , UnitId(..)
  , Version(..)
  )
import Hgs.Paths ( PackagePath(..) )
import Hgs.PathTrie
  ( pathTrieFromPaths
  , renderPathTrie
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "pathTrieFromPaths" $ do
    it "compresses common reversed suffixes" $ do
      let rendered =
            maybe "" renderPathTrie $
              pathTrieFromPaths
                [ PackagePath [rootA, parent, target]
                , PackagePath [rootB, parent, target]
                ]

      rendered `shouldContain` "target-1.0.0"
      rendered `shouldContain` "parent-1.0.0"
      rendered `shouldContain` "root-a-1.0.0"
      rendered `shouldContain` "root-b-1.0.0"

      countLinesContaining "parent-1.0.0" rendered `shouldBe` 1

    it "returns Nothing for no paths" $ do
      pathTrieFromPaths [] `shouldBe` Nothing

countLinesContaining :: String -> String -> Int
countLinesContaining needle =
  length . filter (needle `isInfixOf`) . lines

target :: Package
target =
  pkg "target-id" "target" "1.0.0"

parent :: Package
parent =
  pkg "parent-id" "parent" "1.0.0"

rootA :: Package
rootA =
  localPkg "root-a-id" "root-a" "1.0.0"

rootB :: Package
rootB =
  localPkg "root-b-id" "root-b" "1.0.0"

pkg :: Text -> Text -> Text -> Package
pkg unitId name version =
  Package
    { packageUnitId = UnitId unitId
    , packageName = PackageName name
    , packageVersion = Version version
    , packageSource = PackageExternal
    , packageSourcePath = Nothing
    , packageDepends = mempty
    , packageIsDirect = False
    }

localPkg :: Text -> Text -> Text -> Package
localPkg unitId name version =
  (pkg unitId name version)
    { packageSource = PackageLocal
    }

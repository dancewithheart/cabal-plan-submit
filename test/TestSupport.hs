{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TestSupport
  ( SimplePlan(..)
  , snapshotInput
  , simpleGraph
  , mkLocal
  , mkExternal
  , mkExternalWith
  , toText
  , expectObject
  , valueKey
  ) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (zipWith4)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Hgs.Domain
  ( PackageName(..)
  , PlanGraph
  , RawPkgSrc(..)
  , RawPlan(..)
  , RawPlanItem(..)
  , UnitId(..)
  , Version(..)
  )
import Hgs.Extract (extractPlanGraph)
import Hgs.Snapshot (SnapshotInput(..))
import Test.QuickCheck

newtype SimplePlan = SimplePlan RawPlan
  deriving stock (Show)

instance Arbitrary SimplePlan where
  arbitrary = do
    names <- listOf1 genBaseName
    let unitIds = mkUnitIds names
    depLists <- vectorOf (length unitIds) (listOf (elements (unknownUnitId : unitIds)))
    localFlags <- vectorOf (length unitIds) arbitrary
    pure $
      SimplePlan $
        RawPlan
          { rawPlanCabalVersion = Nothing
          , rawPlanCompilerId = Nothing
          , rawPlanItems =
              zipWith4 mkItem unitIds depLists localFlags [1 :: Int ..]
          }

genBaseName :: Gen String
genBaseName =
  listOf1 (elements ['a' .. 'z'])

mkUnitIds :: [String] -> [UnitId]
mkUnitIds =
  map (\n -> UnitId (toText (n <> "-1.0.0")))

mkItem :: UnitId -> [UnitId] -> Bool -> Int -> RawPlanItem
mkItem unitId deps isLocal n =
  RawPlanItem
    { rawPlanItemType = Just "configured"
    , rawPlanItemId = Just unitId
    , rawPlanItemPkgName = Just (PackageName (toText ("pkg" <> show n)))
    , rawPlanItemPkgVersion = Just (Version "1.0.0")
    , rawPlanItemDepends = deps
    , rawPlanItemPkgSrc =
        if isLocal
          then Just (RawPkgSrc (Just "local") (Just "."))
          else Nothing
    }

mkLocal :: Text.Text -> [UnitId] -> RawPlanItem
mkLocal unitId deps =
  RawPlanItem
    { rawPlanItemType = Just "configured"
    , rawPlanItemId = Just (UnitId unitId)
    , rawPlanItemPkgName = Just (PackageName "root")
    , rawPlanItemPkgVersion = Just (Version "0.1.0.0")
    , rawPlanItemDepends = deps
    , rawPlanItemPkgSrc = Just (RawPkgSrc (Just "local") (Just "."))
    }

mkExternal :: Text.Text -> Text.Text -> Text.Text -> RawPlanItem
mkExternal unitId name version =
  mkExternalWith unitId name version []

mkExternalWith :: Text.Text -> Text.Text -> Text.Text -> [UnitId] -> RawPlanItem
mkExternalWith unitId name version deps =
  RawPlanItem
    { rawPlanItemType = Just "configured"
    , rawPlanItemId = Just (UnitId unitId)
    , rawPlanItemPkgName = Just (PackageName name)
    , rawPlanItemPkgVersion = Just (Version version)
    , rawPlanItemDepends = deps
    , rawPlanItemPkgSrc = Nothing
    }

unknownUnitId :: UnitId
unknownUnitId =
  UnitId "unknown-9.9.9"

toText :: String -> Text.Text
toText =
  Text.pack

simpleGraph :: PlanGraph
simpleGraph =
  extractPlanGraph $
    RawPlan
      { rawPlanCabalVersion = Nothing
      , rawPlanCompilerId = Nothing
      , rawPlanItems =
          [ mkLocal "mypkg-0.1.0.0-inplace" [UnitId "aeson-2.2.4.1", UnitId "text-2.0.2"]
          , mkExternalWith "aeson-2.2.4.1" "aeson" "2.2.4.1" [UnitId "bytestring-0.11.5.3", UnitId "text-2.0.2"]
          , mkExternalWith "text-2.0.2" "text" "2.0.2" [UnitId "bytestring-0.11.5.3"]
          , mkExternal "bytestring-0.11.5.3" "bytestring" "0.11.5.3"
          ]
      }

snapshotInput :: SnapshotInput
snapshotInput =
  SnapshotInput
    { snapshotSha = "0123456789abcdef0123456789abcdef01234567"
    , snapshotRef = "refs/heads/main"
    , snapshotScannedAt = UTCTime (toEnum 0) (secondsToDiffTime 0)
    , snapshotJobId = "test"
    , snapshotCorrelator = "test"
    , snapshotManifestKey = "cabal-project"
    , snapshotManifestName = "cabal project"
    , snapshotManifestPath = Just "cabal.project"
    , snapshotDetectorName = "cabal-plan-submit"
    , snapshotDetectorVersion = "0.1.0.1"
    , snapshotDetectorUrl = "https://github.com/dancewithheart/cabal-plan-submit"
    }

valueKey :: Text.Text -> Aeson.Value -> Aeson.Value
valueKey key =
  \case
    Aeson.Object o ->
      case KeyMap.lookup (Key.fromText key) o of
        Just v -> v
        Nothing -> error ("missing key: " <> Text.unpack key)
    _ ->
      error "expected object"

expectObject :: Aeson.Value -> KeyMap.KeyMap Aeson.Value
expectObject =
  \case
    Aeson.Object o -> o
    _ -> error "expected object"


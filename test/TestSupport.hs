{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TestSupport
  ( SimplePlan(..)
  , snapshotInput
  , simpleGraph
  , graphFromValidPlan
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
import Data.List (zipWith6)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Hgs.Domain
  ( ComponentName(..)
  , PackageName(..)
  , PlanGraph
  , RawPkgSrc(..)
  , RawPlan(..)
  , RawPlanItem(..)
  , RawComponent(..)
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
    names <- genBaseName 20
    let unitIds = mkUnitIds names
    depLists <- vectorOf (length unitIds) (listOf (elements unitIds))
    exeDepLists <- vectorOf (length unitIds) (listOf (elements unitIds))
    localFlags <- vectorOf (length unitIds) arbitrary
    packageNames <- vectorOf (length unitIds) (elements names)
    componentLists <- vectorOf (length unitIds) $ do
      hasComponent <- arbitrary
      componentDeps <- listOf (elements unitIds)
      componentTools <- listOf (elements unitIds)
      componentName <- elements ["lib", "test:spec", "bench:speed"]
      pure [RawComponent (ComponentName componentName) componentDeps componentTools | hasComponent]
    pure $
      SimplePlan $
        RawPlan
          { rawPlanCabalVersion = Nothing
          , rawPlanCompilerId = Nothing
          , rawPlanOS = Nothing
          , rawPlanArch = Nothing
          , rawPlanItems =
              zipWith6 mkItem unitIds depLists exeDepLists localFlags packageNames componentLists
          }

genBaseName :: Int -> Gen [String]
genBaseName maxN = do
      n <- chooseInt (1, maxN)
      let names   = [ "pkg" <> show i | i <- [1 .. n] ]
      return names

mkUnitIds :: [String] -> [UnitId]
mkUnitIds =
  map (\n -> UnitId (toText (n <> "-1.0.0")))

mkItem :: UnitId -> [UnitId] -> [UnitId] -> Bool -> String -> [RawComponent] -> RawPlanItem
mkItem unitId deps exeDeps isLocal name components =
  RawPlanItem
    { rawPlanItemType = Just "configured"
    , rawPlanItemId = Just unitId
    , rawPlanItemPkgName = Just (PackageName (toText name))
    , rawPlanItemPkgVersion = Just (Version "1.0.0")
    , rawPlanItemComponents = components
    , rawPlanItemExeDepends = exeDeps
    , rawPlanItemComponentName = Nothing
    , rawPlanItemDepends = deps
    , rawPlanItemFlags = Nothing
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
    , rawPlanItemComponents = []
    , rawPlanItemExeDepends = []
    , rawPlanItemComponentName = Just (ComponentName "lib")
    , rawPlanItemFlags = Nothing
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
    , rawPlanItemComponents = []
    , rawPlanItemExeDepends = []
    , rawPlanItemComponentName = Nothing
    , rawPlanItemFlags = Nothing
    , rawPlanItemPkgSrc = Nothing
    }

toText :: String -> Text.Text
toText =
  Text.pack

simpleGraph :: PlanGraph
simpleGraph =
  graphFromValidPlan $
    RawPlan
      { rawPlanCabalVersion = Nothing
      , rawPlanCompilerId = Nothing
      , rawPlanOS = Nothing
      , rawPlanArch = Nothing
      , rawPlanItems =
          [ mkLocal "mypkg-0.1.0.0-inplace" [UnitId "aeson-2.2.4.1", UnitId "text-2.0.2"]
          , mkExternalWith "aeson-2.2.4.1" "aeson" "2.2.4.1" [UnitId "bytestring-0.11.5.3", UnitId "text-2.0.2"]
          , mkExternalWith "text-2.0.2" "text" "2.0.2" [UnitId "bytestring-0.11.5.3"]
          , mkExternal "bytestring-0.11.5.3" "bytestring" "0.11.5.3"
          ]
      }

graphFromValidPlan :: RawPlan -> PlanGraph
graphFromValidPlan = either (error . show) id . extractPlanGraph

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


{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module PlanJsonSpec (spec) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Either (isLeft, isRight)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Hgs.Domain
  (
  ComponentName(..)
  , Package(..)
  , PackageName(..)
  , PackageSource(..)
  , PlanGraph(..)
  , RawComponent(..)
  , RawPkgSrc(..)
  , RawPlan(..)
  , RawPlanItem(..)
  , UnitId(..)
  , Version(..)
  )
import Hgs.Extract (PlanGraphError(..), extractPlanGraph, extractPlanGraphChecked)
import Hgs.Input.PlanJson (decodeRawPlan)
import Hgs.Snapshot
  ( snapshotFromPlanGraph
  )
import Test.Hspec
import Test.QuickCheck
import TestSupport (SimplePlan(..), expectObject, mkExternalWith, mkLocal, simpleGraph, snapshotInput, valueKey)

spec :: Spec
spec = do
  describe "decodeRawPlan" $ do
    it "requires an install-plan array while allowing an empty one" $ do
      map decodeRawPlan ["{}", "{\"install-plan\":null}", "{\"install-plan\":{}}"]
        `shouldSatisfy` all isLeft
      decodeRawPlan "{\"install-plan\":[]}" `shouldSatisfy` isRight

    it "parses a minimal plan with one install-plan entry" $ do
      let input =
            "{\
            \  \"cabal-lib-version\": \"3.10.1.0\",\
            \  \"compiler-id\": \"ghc-9.8.2\",\
            \  \"install-plan\": [\
            \    {\
            \      \"type\": \"pre-existing\",\
            \      \"id\": \"base-4.19.1.0\",\
            \      \"pkg-name\": \"base\",\
            \      \"pkg-version\": \"4.19.1.0\",\
            \      \"depends\": []\
            \    }\
            \  ]\
            \}"
      case decodeRawPlan input of
        Left err -> expectationFailure err
        Right plan -> do
          length (rawPlanItems plan) `shouldBe` 1
          rawPlanCompilerId plan `shouldBe` Just "ghc-9.8.2"

    it "tolerates missing optional fields" $ do
      let input =
            "{\
            \  \"install-plan\": [\
            \    {\
            \      \"id\": \"bytestring-0.12.1.0\"\
            \    }\
            \  ]\
            \}"
      case decodeRawPlan input of
        Left err -> expectationFailure err
        Right plan -> length (rawPlanItems plan) `shouldBe` 1

    it "preserves component names and component dependency sets" $ do
      let input =
            "{\
            \  \"install-plan\": [\
            \    {\
            \      \"type\": \"configured\",\
            \      \"id\": \"unix-time-0.4.17-inplace\",\
            \      \"pkg-name\": \"unix-time\",\
            \      \"pkg-version\": \"0.4.17\",\
            \      \"pkg-src\": { \"type\": \"local\", \"path\": \".\" },\
            \      \"depends\": null,\
            \      \"components\": {\
            \        \"lib\": {\
            \          \"depends\": [\
            \            \"base-4.18.3.0\",\
            \            \"old-time-1.1.1.0-oldtimehash\"\
            \          ]\
            \        },\
            \        \"test:spec\": {\
            \          \"depends\": [\
            \            \"hspec-2.11.17-hspechash\",\
            \            \"old-time-1.1.1.0-oldtimehash\"\
            \          ]\
            \        }\
            \      }\
            \    }\
            \  ]\
            \}"

      case decodeRawPlan input of
        Left err -> expectationFailure err
        Right plan ->
          case rawPlanItems plan of
            [item] -> do
              fmap rawComponentName (rawPlanItemComponents item)
                `shouldBe`
                  [ ComponentName "lib"
                  , ComponentName "test:spec"
                  ]

              fmap rawComponentDepends (rawPlanItemComponents item)
                `shouldBe`
                  [ [ UnitId "base-4.18.3.0"
                    , UnitId "old-time-1.1.1.0-oldtimehash"
                    ]
                  , [ UnitId "hspec-2.11.17-hspechash"
                    , UnitId "old-time-1.1.1.0-oldtimehash"
                    ]
                  ]

            _ ->
              expectationFailure "expected exactly one plan item"

    it "preserves local package source paths" $ do
      let local =
            RawPlanItem
              { rawPlanItemType = Just "configured"
              , rawPlanItemId = Just (UnitId "local-1")
              , rawPlanItemPkgName = Just (PackageName "local")
              , rawPlanItemPkgVersion = Just (Version "1")
              , rawPlanItemDepends = []
              , rawPlanItemComponents = []
              , rawPlanItemPkgSrc = Just (RawPkgSrc (Just "local") (Just "packages/local"))
              }
          plan = RawPlan Nothing Nothing [local]
          packages = planGraphPackages (extractPlanGraph plan)
      fmap packageSourcePath (Map.lookup (UnitId "local-1") packages)
        `shouldBe` Just (Just "packages/local")

    it "parses component dependencies when top-level depends is null" $ do
      let input =
            "{\
            \  \"install-plan\": [\
            \    {\
            \      \"type\": \"configured\",\
            \      \"id\": \"unix-time-0.4.17-inplace\",\
            \      \"pkg-name\": \"unix-time\",\
            \      \"pkg-version\": \"0.4.17\",\
            \      \"pkg-src\": { \"type\": \"local\", \"path\": \".\" },\
            \      \"depends\": null,\
            \      \"components\": {\
            \        \"lib\": {\
            \          \"depends\": [\
            \            \"base-4.18.3.0\",\
            \            \"old-time-1.1.1.0-oldtimehash\"\
            \          ]\
            \        },\
            \        \"test:spec\": {\
            \          \"depends\": [\
            \            \"hspec-2.11.17-hspechash\",\
            \            \"old-time-1.1.1.0-oldtimehash\"\
            \          ]\
            \        }\
            \      }\
            \    },\
            \    {\
            \      \"type\": \"configured\",\
            \      \"id\": \"old-time-1.1.1.0-oldtimehash\",\
            \      \"pkg-name\": \"old-time\",\
            \      \"pkg-version\": \"1.1.1.0\",\
            \      \"pkg-src\": { \"type\": \"repo-tar\" },\
            \      \"depends\": null\
            \    },\
            \    {\
            \      \"type\": \"pre-existing\",\
            \      \"id\": \"base-4.18.3.0\",\
            \      \"pkg-name\": \"base\",\
            \      \"pkg-version\": \"4.18.3.0\",\
            \      \"depends\": []\
            \    },\
            \    {\
            \      \"type\": \"configured\",\
            \      \"id\": \"hspec-2.11.17-hspechash\",\
            \      \"pkg-name\": \"hspec\",\
            \      \"pkg-version\": \"2.11.17\",\
            \      \"pkg-src\": { \"type\": \"repo-tar\" },\
            \      \"depends\": []\
            \    }\
            \  ]\
            \}"

      case decodeRawPlan input of
        Left err ->
          expectationFailure err
        Right plan -> do
          let graph = extractPlanGraph plan
              packages = planGraphPackages graph

          fmap packageDepends (Map.lookup (UnitId "unix-time-0.4.17-inplace") packages)
            `shouldBe` Just
              ( Set.fromList
                  [ UnitId "base-4.18.3.0"
                  , UnitId "old-time-1.1.1.0-oldtimehash"
                  , UnitId "hspec-2.11.17-hspechash"
                  ]
              )

          fmap packageIsDirect (Map.lookup (UnitId "old-time-1.1.1.0-oldtimehash") packages)
            `shouldBe` Just True

  describe "extractPlanGraph" $ do
    it "marks direct external dependencies from local packages" $ do
      let localPkg =
            RawPlanItem
              { rawPlanItemType = Just "configured"
              , rawPlanItemId = Just (UnitId "mypkg-0.1.0.0-inplace")
              , rawPlanItemPkgName = Just (PackageName "mypkg")
              , rawPlanItemPkgVersion = Just (Version "0.1.0.0")
              , rawPlanItemDepends = [UnitId "aeson-2.2.4.1", UnitId "text-2.0.2"]
              , rawPlanItemComponents = []
              , rawPlanItemPkgSrc = Just (RawPkgSrc (Just "local") (Just "."))
              }

          aesonPkg =
            RawPlanItem
              { rawPlanItemType = Just "configured"
              , rawPlanItemId = Just (UnitId "aeson-2.2.4.1")
              , rawPlanItemPkgName = Just (PackageName "aeson")
              , rawPlanItemPkgVersion = Just (Version "2.2.4.1")
              , rawPlanItemDepends = [UnitId "bytestring-0.11.5.3", UnitId "text-2.0.2"]
              , rawPlanItemComponents = []
              , rawPlanItemPkgSrc = Nothing
              }

          textPkg =
            RawPlanItem
              { rawPlanItemType = Just "configured"
              , rawPlanItemId = Just (UnitId "text-2.0.2")
              , rawPlanItemPkgName = Just (PackageName "text")
              , rawPlanItemPkgVersion = Just (Version "2.0.2")
              , rawPlanItemDepends = [UnitId "bytestring-0.11.5.3"]
              , rawPlanItemComponents = []
              , rawPlanItemPkgSrc = Nothing
              }

          bytestringPkg =
            RawPlanItem
              { rawPlanItemType = Just "configured"
              , rawPlanItemId = Just (UnitId "bytestring-0.11.5.3")
              , rawPlanItemPkgName = Just (PackageName "bytestring")
              , rawPlanItemPkgVersion = Just (Version "0.11.5.3")
              , rawPlanItemDepends = []
              , rawPlanItemComponents = []
              , rawPlanItemPkgSrc = Nothing
              }

          graph =
            extractPlanGraph
              RawPlan
                { rawPlanCabalVersion = Nothing
                , rawPlanCompilerId = Nothing
                , rawPlanItems = [localPkg, aesonPkg, textPkg, bytestringPkg]
                }

          packages = planGraphPackages graph

      fmap packageSource (Map.lookup (UnitId "mypkg-0.1.0.0-inplace") packages)
        `shouldBe` Just PackageLocal

      fmap packageIsDirect (Map.lookup (UnitId "aeson-2.2.4.1") packages)
        `shouldBe` Just True

      fmap packageIsDirect (Map.lookup (UnitId "text-2.0.2") packages)
        `shouldBe` Just True

      fmap packageIsDirect (Map.lookup (UnitId "bytestring-0.11.5.3") packages)
        `shouldBe` Just False

    it "keeps only dependency edges to known packages" $
      property prop_extractGraphKeepsKnownTargets

  describe "extractPlanGraphChecked" $ do
    let plan = RawPlan Nothing Nothing
        root = mkLocal "root"
        dependency = mkExternalWith "dep" "dep" "1"

    it "rejects a dangling dependency before extracting a graph" $
      extractPlanGraphChecked (plan [root [UnitId "missing"]])
        `shouldBe` Left (UnknownDependency (UnitId "root") (UnitId "missing"))

    it "rejects incomplete records and duplicate unit identities" $ do
      extractPlanGraphChecked (plan [(root []) { rawPlanItemPkgVersion = Nothing }])
        `shouldBe` Left (IncompletePlanItem 0)
      extractPlanGraphChecked (plan [root [], root []])
        `shouldBe` Left (DuplicateUnit (UnitId "root"))

    it "preserves cycles and repeated edges in set form" $ do
      let result = extractPlanGraphChecked (plan [root [UnitId "dep", UnitId "dep"], dependency [UnitId "root"]])
      fmap (fmap packageDepends . Map.lookup (UnitId "root") . planGraphPackages) result
        `shouldBe` Right (Just (Set.singleton (UnitId "dep")))
      fmap (fmap packageDepends . Map.lookup (UnitId "dep") . planGraphPackages) result
        `shouldBe` Right (Just (Set.singleton (UnitId "root")))

    it "retains all represented edges in valid plans" $
      property (withNumTests 1000 prop_checkedPreservesEdges)

    it "rejects generated plans with an unknown target" $
      property (withNumTests 1000 prop_checkedRejectsUnknown)

  describe "snapshotFromPlanGraph" $ do
    it "emits only external packages in resolved" $ do
      let graph = simpleGraph
          value = Aeson.toJSON (snapshotFromPlanGraph snapshotInput graph)
          manifests = valueKey "manifests" value
          manifest = valueKey "cabal-project" manifests
          resolved = valueKey "resolved" manifest

      KeyMap.size (expectObject resolved) `shouldBe` 3

    it "keeps resolved dependency references inside resolved keys" $
      property prop_snapshotDependenciesStayInsideResolved

prop_checkedPreservesEdges :: SimplePlan -> Bool
prop_checkedPreservesEdges (SimplePlan raw) =
  case extractPlanGraphChecked valid of
    Left _ -> False
    Right graph ->
      Map.keysSet (planGraphPackages graph) == known && all (preserved graph) items
 where
  known = Set.fromList [uid | item <- rawPlanItems raw, Just uid <- [rawPlanItemId item]]
  items = [item {rawPlanItemDepends = filter (`Set.member` known) (rawPlanItemDepends item)} | item <- rawPlanItems raw]
  valid = raw {rawPlanItems = items}
  preserved graph item =
    case rawPlanItemId item of
      Nothing -> False
      Just uid -> fmap packageDepends (Map.lookup uid (planGraphPackages graph)) == Just (Set.fromList (rawPlanItemDepends item))

prop_checkedRejectsUnknown :: SimplePlan -> Bool
prop_checkedRejectsUnknown (SimplePlan raw) =
  case rawPlanItems raw of
    [] -> False
    item : rest ->
      case extractPlanGraphChecked (raw {rawPlanItems = item {rawPlanItemDepends = UnitId "unknown-9.9.9" : rawPlanItemDepends item} : rest}) of
        Left (UnknownDependency _ _) -> True
        _ -> False

prop_extractGraphKeepsKnownTargets :: SimplePlan -> Bool
prop_extractGraphKeepsKnownTargets (SimplePlan rawPlan) =
  all depsKnown (Map.elems (planGraphPackages graph))
 where
  graph = extractPlanGraph rawPlan
  known = Map.keysSet (planGraphPackages graph)

  depsKnown pkg =
    packageDepends pkg `Set.isSubsetOf` known

prop_snapshotDependenciesStayInsideResolved :: SimplePlan -> Bool
prop_snapshotDependenciesStayInsideResolved (SimplePlan rawPlan) =
  all depsKnown resolvedEntries
 where
  graph = extractPlanGraph rawPlan
  value = Aeson.toJSON (snapshotFromPlanGraph snapshotInput graph)
  manifests = valueKey "manifests" value
  manifest = valueKey "cabal-project" manifests
  resolved = expectObject (valueKey "resolved" manifest)
  resolvedKeys =
    Set.fromList
      [ Key.toText k
      | k <- KeyMap.keys resolved
      ]
  resolvedEntries =
    KeyMap.elems resolved

  depsKnown entry =
    case entry of
      Aeson.Object o ->
        case KeyMap.lookup "dependencies" o of
          Just (Aeson.Array arr) ->
            all (\case Aeson.String t -> t `Set.member` resolvedKeys; _ -> False) arr
          Nothing ->
            False
          _ ->
            False
      _ ->
        False

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module PlanJsonSpec (spec) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Hgs.Domain
  ( Package(..)
  , PackageName(..)
  , PackageSource(..)
  , PlanGraph(..)
  , RawPkgSrc(..)
  , RawPlan(..)
  , RawPlanItem(..)
  , UnitId(..)
  , Version(..)
  )
import Hgs.Extract (extractPlanGraph)
import Hgs.Input.PlanJson (decodeRawPlan)
import Hgs.Snapshot
  ( snapshotFromPlanGraph
  )
import Test.Hspec
import Test.QuickCheck
import TestSupport (SimplePlan(..), expectObject, simpleGraph, snapshotInput, valueKey)

spec :: Spec
spec = do
  describe "decodeRawPlan" $ do
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
        Left err ->
          expectationFailure err
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
        Left err ->
          expectationFailure err
        Right plan ->
          length (rawPlanItems plan) `shouldBe` 1

  describe "extractPlanGraph" $ do
    it "marks direct external dependencies from local packages" $ do
      let localPkg =
            RawPlanItem
              { rawPlanItemType = Just "configured"
              , rawPlanItemId = Just (UnitId "mypkg-0.1.0.0-inplace")
              , rawPlanItemPkgName = Just (PackageName "mypkg")
              , rawPlanItemPkgVersion = Just (Version "0.1.0.0")
              , rawPlanItemDepends = [UnitId "aeson-2.2.4.1", UnitId "text-2.0.2"]
              , rawPlanItemPkgSrc = Just (RawPkgSrc (Just "local") (Just "."))
              }

          aesonPkg =
            RawPlanItem
              { rawPlanItemType = Just "configured"
              , rawPlanItemId = Just (UnitId "aeson-2.2.4.1")
              , rawPlanItemPkgName = Just (PackageName "aeson")
              , rawPlanItemPkgVersion = Just (Version "2.2.4.1")
              , rawPlanItemDepends = [UnitId "bytestring-0.11.5.3", UnitId "text-2.0.2"]
              , rawPlanItemPkgSrc = Nothing
              }

          textPkg =
            RawPlanItem
              { rawPlanItemType = Just "configured"
              , rawPlanItemId = Just (UnitId "text-2.0.2")
              , rawPlanItemPkgName = Just (PackageName "text")
              , rawPlanItemPkgVersion = Just (Version "2.0.2")
              , rawPlanItemDepends = [UnitId "bytestring-0.11.5.3"]
              , rawPlanItemPkgSrc = Nothing
              }

          bytestringPkg =
            RawPlanItem
              { rawPlanItemType = Just "configured"
              , rawPlanItemId = Just (UnitId "bytestring-0.11.5.3")
              , rawPlanItemPkgName = Just (PackageName "bytestring")
              , rawPlanItemPkgVersion = Just (Version "0.11.5.3")
              , rawPlanItemDepends = []
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

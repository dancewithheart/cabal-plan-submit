{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module PlanJsonSpec (spec) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Either (isLeft, isRight)
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Hgs.Domain
  (
  ComponentName(..)
  , DependencyKind(..)
  , DependencyOrigin(..)
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
  , packageDepends
  )
import Hgs.Extract (PlanGraphError(..), extractPlanGraph, extractPlanGraphChecked)
import Hgs.Input.PlanJson (decodeRawPlan)
import Hgs.LocalUnitFilter (LocalUnitFilter(..), localUnitAllowed)
import Hgs.Snapshot
  ( snapshotFromPlanGraph
  )
import Hgs.Why (shortestPathsToPackage, shortestPathsToPackageFrom)
import Test.Hspec
import Test.QuickCheck
import TestSupport (graphFromValidPlan, SimplePlan(..), expectObject, mkExternalWith, mkLocal, simpleGraph, snapshotInput, valueKey)

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

    it "rejects malformed top-level and component executable dependencies" $ do
      let invalid =
            [ "{\"install-plan\":[{\"exe-depends\":\"wrong\"}]}"
            , "{\"install-plan\":[{\"components\":{\"lib\":{\"exe-depends\":[42]}}}]}"
            ]
      map decodeRawPlan invalid `shouldSatisfy` all isLeft

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
              , rawPlanItemExeDepends = []
              , rawPlanItemComponentName = Nothing
              , rawPlanItemPkgSrc = Just (RawPkgSrc (Just "local") (Just "packages/local"))
              }
          plan = RawPlan Nothing Nothing [local]
          packages = planGraphPackages (graphFromValidPlan plan)
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
          let graph = graphFromValidPlan plan
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
    it "rejects an unknown build-tool edge instead of silently dropping it" $ do
      let item = (mkLocal "root" []) {rawPlanItemExeDepends = [UnitId "missing-tool"]}
      extractPlanGraph (RawPlan Nothing Nothing [item])
        `shouldBe` Left (UnknownDependency (UnitId "root") (UnitId "missing-tool"))

    it "marks direct external dependencies from local packages" $ do
      let localPkg =
            RawPlanItem
              { rawPlanItemType = Just "configured"
              , rawPlanItemId = Just (UnitId "mypkg-0.1.0.0-inplace")
              , rawPlanItemPkgName = Just (PackageName "mypkg")
              , rawPlanItemPkgVersion = Just (Version "0.1.0.0")
              , rawPlanItemDepends = [UnitId "aeson-2.2.4.1", UnitId "text-2.0.2"]
              , rawPlanItemComponents = []
              , rawPlanItemExeDepends = []
              , rawPlanItemComponentName = Nothing
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
              , rawPlanItemExeDepends = []
              , rawPlanItemComponentName = Nothing
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
              , rawPlanItemExeDepends = []
              , rawPlanItemComponentName = Nothing
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
              , rawPlanItemExeDepends = []
              , rawPlanItemComponentName = Nothing
              , rawPlanItemPkgSrc = Nothing
              }

          graph =
            graphFromValidPlan
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

    it "only accepts graphs whose dependency targets are known" $
      property prop_extractGraphKeepsKnownTargets

    it "never introduces reachability when selecting production components" $
      property prop_productionReachabilitySubset

    it "retains unknown component provenance instead of hiding a possible production path" $ do
      let root = (mkLocal "root-test" [UnitId "dep"]) {rawPlanItemComponentName = Nothing}
          graph = graphFromValidPlan (RawPlan Nothing Nothing [root, mkExternalWith "dep" "dep" "1" []])
      shortestPathsToPackageFrom ProductionLocalUnits (PackageName "dep") graph `shouldSatisfy` (not . null)

  describe "extractPlanGraphChecked" $ do
    let plan = RawPlan Nothing Nothing
        root = mkLocal "root"
        dependency = mkExternalWith "dep" "dep" "1"

    it "rejects a dangling dependency before extracting a graph" $
      extractPlanGraphChecked (plan [root [UnitId "missing"]])
        `shouldBe` Left (UnknownDependency (UnitId "root") (UnitId "missing"))

    it "rejects a dangling executable dependency" $
      extractPlanGraphChecked (plan [(root []) { rawPlanItemExeDepends = [UnitId "missing"] }])
        `shouldBe` Left (UnknownDependency (UnitId "root") (UnitId "missing"))

    it "retains every kind and component when an edge has multiple origins" $ do
      let item = (root [UnitId "dep"])
            { rawPlanItemComponentName = Nothing
            , rawPlanItemExeDepends = [UnitId "dep"]
            , rawPlanItemComponents = [RawComponent (ComponentName "test:spec") [UnitId "dep"] [UnitId "dep"]]
            }
          result = extractPlanGraphChecked (plan [item, dependency []])
          origins graph = do
            pkg <- Map.lookup (UnitId "root") (planGraphPackages graph)
            Map.lookup (UnitId "dep") (packageDependencies pkg)
      fmap origins result
        `shouldBe` Right (Just (Set.fromList
          [ DependencyOrigin Nothing ComponentDependency
          , DependencyOrigin Nothing ExecutableDependency
          , DependencyOrigin (Just (ComponentName "test:spec")) ComponentDependency
          , DependencyOrigin (Just (ComponentName "test:spec")) ExecutableDependency
          ]))

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
      property (withMaxSuccess 1000 prop_checkedPreservesEdges)

    it "rejects generated plans with an unknown target" $
      property (withMaxSuccess 1000 prop_checkedRejectsUnknown)

  describe "real Cabal executable dependencies" $ do
    it "links local unix-time to its hsc2hs build tool" $ do
      graph <- graphFromFixture "test/golden/unix-time/plan.json"
      let root = packageNamed "unix-time" graph
          tool = packageNamed "hsc2hs" graph
      packageUnitId tool `Set.member` packageDepends root `shouldBe` True
      packageIsDirect tool `shouldBe` True
      Map.lookup (packageUnitId tool) (packageDependencies root)
        `shouldBe` Just (Set.singleton (DependencyOrigin (Just (ComponentName "lib")) ExecutableDependency))
      shortestPathsToPackage (PackageName "hsc2hs") graph `shouldSatisfy` (not . null)

    it "links persistent's network library to hsc2hs" $ do
      graph <- graphFromFixture "test/golden/persistent/plan.json"
      let network = packageNamed "network" graph
          tool = packageNamed "hsc2hs" graph
      packageUnitId tool `Set.member` packageDepends network `shouldBe` True
      let snapshot = Aeson.toJSON (snapshotFromPlanGraph snapshotInput graph)
          resolved = valueKey "resolved" (valueKey "cabal-project" (valueKey "manifests" snapshot))
          networkPurl = "pkg:hackage/network@" <> unVersion (packageVersion network)
          entry = valueKey networkPurl resolved
      case valueKey "dependencies" entry of
        Aeson.Array dependencies -> Vector.toList dependencies `shouldContain` [Aeson.String "pkg:hackage/hsc2hs@0.68.10"]
        _ -> expectationFailure "expected network dependencies array"

    it "retains setup-component provenance from persistent's entropy package" $ do
      graph <- graphFromFixture "test/golden/persistent/plan.json"
      let entropy = packageNamed "entropy" graph
          cabal = packageNamed "Cabal" graph
      Map.lookup (packageUnitId cabal) (packageDependencies entropy)
        `shouldBe` Just (Set.singleton (DependencyOrigin (Just (ComponentName "setup")) ComponentDependency))

    it "does not claim runtime scope for unix-time's test-only hspec dependency" $ do
      graph <- graphFromFixture "test/golden/unix-time/plan.json"
      let root = packageNamed "unix-time" graph
          hspecPkg = packageNamed "hspec" graph
          snapshot = Aeson.toJSON (snapshotFromPlanGraph snapshotInput graph)
          resolved = valueKey "resolved" (valueKey "cabal-project" (valueKey "manifests" snapshot))
          hspecPurl = "pkg:hackage/hspec@" <> unVersion (packageVersion hspecPkg)
          entry = expectObject (valueKey hspecPurl resolved)
      Map.lookup (packageUnitId hspecPkg) (packageDependencies root)
        `shouldBe` Just (Set.singleton (DependencyOrigin (Just (ComponentName "test:spec")) ComponentDependency))
      KeyMap.lookup "scope" entry `shouldBe` Just (Aeson.String "development")

    it "excludes unix-time's test-only hspec from production paths" $ do
      graph <- graphFromFixture "test/golden/unix-time/plan.json"
      shortestPathsToPackageFrom AllLocalUnits (PackageName "hspec") graph `shouldSatisfy` (not . null)
      shortestPathsToPackageFrom ProductionLocalUnits (PackageName "hspec") graph `shouldBe` []
      shortestPathsToPackageFrom ProductionLocalUnits (PackageName "hsc2hs") graph `shouldSatisfy` (not . null)

    it "recognizes a test component even when its unit id lacks a test suffix" $ do
      graph <- graphFromFixture "test/golden/persistent/plan.json"
      let unit = UnitId "persistent-redis-2.13.0.2-inplace-basic"
      fmap (localUnitAllowed ProductionLocalUnits) (Map.lookup unit (planGraphPackages graph))
        `shouldBe` Just False

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
  items = map keepKnown (rawPlanItems raw)
  keepKnown item = item
    { rawPlanItemDepends = onlyKnown (rawPlanItemDepends item)
    , rawPlanItemExeDepends = onlyKnown (rawPlanItemExeDepends item)
    , rawPlanItemComponents = map keepKnownComponent (rawPlanItemComponents item)
    }
  keepKnownComponent component = component
    { rawComponentDepends = onlyKnown (rawComponentDepends component)
    , rawComponentExeDepends = onlyKnown (rawComponentExeDepends component)
    }
  onlyKnown = filter (`Set.member` known)
  valid = raw {rawPlanItems = items}
  preserved graph item =
    case rawPlanItemId item of
      Nothing -> False
      Just uid -> case Map.lookup uid (planGraphPackages graph) of
        Nothing -> False
        Just pkg ->
          let expected = Set.fromList (itemOrigins item)
              actual = Set.fromList
                [(dependency, origin) | (dependency, origins) <- Map.toList (packageDependencies pkg), origin <- Set.toList origins]
           in actual == expected

itemOrigins :: RawPlanItem -> [(UnitId, DependencyOrigin)]
itemOrigins item =
  withOrigin (rawPlanItemComponentName item) ComponentDependency (rawPlanItemDepends item)
    <> withOrigin (rawPlanItemComponentName item) ExecutableDependency (rawPlanItemExeDepends item)
    <> concatMap componentOrigins (rawPlanItemComponents item)
 where
  componentOrigins component =
    withOrigin (Just (rawComponentName component)) ComponentDependency (rawComponentDepends component)
      <> withOrigin (Just (rawComponentName component)) ExecutableDependency (rawComponentExeDepends component)
  withOrigin component kind = map (, DependencyOrigin component kind)

graphFromFixture :: FilePath -> IO PlanGraph
graphFromFixture path = do
  bytes <- BS.readFile path
  case decodeRawPlan bytes >>= either (Left . show) Right . extractPlanGraphChecked of
    Left err -> expectationFailure err *> fail "invalid plan fixture"
    Right graph -> pure graph

packageNamed :: Text.Text -> PlanGraph -> Package
packageNamed name graph =
  case find ((== PackageName name) . packageName) (Map.elems (planGraphPackages graph)) of
    Just pkg -> pkg
    Nothing -> error ("package not found in fixture: " <> Text.unpack name)

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
  graph = graphFromValidPlan rawPlan
  known = Map.keysSet (planGraphPackages graph)

  depsKnown pkg =
    packageDepends pkg `Set.isSubsetOf` known

prop_productionReachabilitySubset :: SimplePlan -> Bool
prop_productionReachabilitySubset (SimplePlan rawPlan) =
  all includedInAll names
 where
  graph = graphFromValidPlan rawPlan
  names = Set.toList (Set.fromList (map packageName (Map.elems (planGraphPackages graph))))
  includedInAll name =
    null (shortestPathsToPackageFrom ProductionLocalUnits name graph)
      || not (null (shortestPathsToPackageFrom AllLocalUnits name graph))

prop_snapshotDependenciesStayInsideResolved :: SimplePlan -> Bool
prop_snapshotDependenciesStayInsideResolved (SimplePlan rawPlan) =
  all depsKnown resolvedEntries
 where
  graph = graphFromValidPlan rawPlan
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

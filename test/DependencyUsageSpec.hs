{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DependencyUsageSpec (spec) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Hgs.DependencyUsage
import Hgs.Domain
import Hgs.Input.PlanJson (decodeRawPlan)
import Test.Hspec
import Test.QuickCheck
import TestSupport (SimplePlan(..), graphFromValidPlan, mkExternal, mkLocal)

spec :: Spec
spec = describe "dependency usage" $ do
  it "distinguishes test, benchmark, production and build-tool paths" $ do
    let root = (mkLocal "root" [])
          { rawPlanItemComponentName = Nothing
          , rawPlanItemComponents =
              [ RawComponent (ComponentName "lib") [UnitId "runtime"] [UnitId "tool"]
              , RawComponent (ComponentName "test:spec") [UnitId "test"] []
              , RawComponent (ComponentName "bench:speed") [UnitId "bench"] []
              ]
          }
        graph = graphFromValidPlan $ RawPlan Nothing Nothing Nothing Nothing
          (root : [mkExternal name name "1" | name <- ["runtime", "tool", "test", "bench"]])
        uses = dependencyUsages graph
    Map.lookup (UnitId "runtime") uses `shouldBe` Just (Set.singleton Production)
    Map.lookup (UnitId "tool") uses `shouldBe` Just (Set.singleton Build)
    Map.lookup (UnitId "test") uses `shouldBe` Just (Set.singleton Test)
    Map.lookup (UnitId "bench") uses `shouldBe` Just (Set.singleton Benchmark)

  it "keeps unknown provenance separate from production" $ do
    let root = (mkLocal "root" [UnitId "dep"]) {rawPlanItemComponentName = Nothing}
        graph = graphFromValidPlan (RawPlan Nothing Nothing Nothing Nothing [root, mkExternal "dep" "dep" "1"])
    Map.lookup (UnitId "dep") (dependencyUsages graph) `shouldBe` Just (Set.singleton Unknown)
    snapshotScope (Set.singleton Unknown) `shouldBe` Nothing

  it "joins usage of distinct units of the same package version" $ do
    let root = (mkLocal "root" [])
          { rawPlanItemComponents =
              [ RawComponent (ComponentName "lib") [UnitId "prod"] []
              , RawComponent (ComponentName "test:spec") [UnitId "dev"] []
              ]
          }
        graph = graphFromValidPlan $ RawPlan Nothing Nothing Nothing Nothing
          [root, mkExternal "prod" "shared" "1", mkExternal "dev" "shared" "1"]
        uses = Map.findWithDefault Set.empty (PackageName "shared", Version "1") (packageUsages graph)
    uses `shouldBe` Set.fromList [Production, Test]
    snapshotScope uses `shouldBe` Just "runtime"

  it "classifies unix-time's hspec as test and hsc2hs as build" $ do
    bytes <- BS.readFile "test/golden/unix-time/plan.json"
    plan <- either (\err -> expectationFailure err >> fail "invalid fixture") pure (decodeRawPlan bytes)
    let graph = graphFromValidPlan plan
        usages = packageUsages graph
    Map.lookup (PackageName "hspec", Version "2.11.17") usages `shouldBe` Just (Set.singleton Test)
    Map.lookup (PackageName "hsc2hs", Version "0.68.10") usages `shouldBe` Just (Set.singleton Build)

  it "does not lose path evidence when an edge is added" $
    property prop_usageMonotone

  it "aggregates unit evidence as a union homomorphism" $
    property prop_packageUsageJoin

  it "classifies a known production path even when other evidence is added" $
    property $ \(uses :: [Bool]) ->
      snapshotScope (Set.fromList (Production : [if x then Test else Unknown | x <- uses])) == Just "runtime"

prop_usageMonotone :: SimplePlan -> Bool
prop_usageMonotone (SimplePlan raw) = case rawPlanItems raw of
  [] -> True
  first : rest -> case rawPlanItemId first of
    Nothing -> True
    Just target ->
      let original = dependencyUsages (graphFromValidPlan raw)
          extended = raw {rawPlanItems = first {rawPlanItemDepends = target : rawPlanItemDepends first} : rest}
          after = dependencyUsages (graphFromValidPlan extended)
       in all (\(unit, uses) -> uses `Set.isSubsetOf` Map.findWithDefault Set.empty unit after) (Map.toList original)

prop_packageUsageJoin :: SimplePlan -> [Bool] -> Bool
prop_packageUsageJoin (SimplePlan raw) choices =
  packageUsagesFrom graph (Map.unionWith Set.union left right)
    == Map.unionWith Set.union (packageUsagesFrom graph left) (packageUsagesFrom graph right)
 where
  graph = graphFromValidPlan raw
  units = Map.keys (planGraphPackages graph)
  tagged = zip units (cycle (choices <> [True]))
  left = Map.fromList [(unit, Set.singleton Production) | (unit, True) <- tagged]
  right = Map.fromList [(unit, Set.singleton (if choice then Test else Benchmark)) | (unit, choice) <- tagged]

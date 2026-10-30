{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module SnapshotSpec (spec) where

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
  , packageDepends
  )
import Hgs.Extract (extractPlanGraph)
import Hgs.Snapshot
  ( snapshotFromPlanGraph
  )
import Hgs.Validate (validateSnapshotValue, getValidationErrors)
import Test.Hspec
import Test.QuickCheck
import TestSupport (SimplePlan(..), expectObject, snapshotInput, valueKey)

spec :: Spec
spec = do
  describe "snapshot invariants" $ do
    it "has no self-PURL edges" $
      property prop_snapshotHasNoSelfPurlEdges

    it "has closure over resolved dependencies" $
      property prop_snapshotHasClosure

    it "has no duplicate dependency PURLs" $
      property prop_snapshotHasUniqueDependencies

    it "marks direct external dependencies soundly" $
      property prop_directnessSoundness

    it "deduplicates dependency PURLs without losing sublibrary edges" $ do
      let rawPlan =
            RawPlan
              { rawPlanCabalVersion = Nothing
              , rawPlanCompilerId = Nothing
              , rawPlanItems =
                  [ mkLocal "root-0.1.0.0-inplace" [UnitId "vector-A"]
                  , mkExternalWith "vector-A" "vector" "0.13.2.0" [UnitId "base-A", UnitId "base-B", UnitId "vector-B"]
                  , mkExternalWith "vector-B" "vector" "0.13.2.0" [UnitId "text-2.0.2"]
                  , mkExternal "base-A" "base" "4.18.3.0"
                  , mkExternal "base-B" "base" "4.18.3.0"
                  , mkExternal "text-2.0.2" "text" "2.0.2"
                  ]
              }
          value = Aeson.toJSON (snapshotFromPlanGraph snapshotInput (extractPlanGraph rawPlan))
          resolved = expectObject (valueKey "resolved" (valueKey "cabal-project" (valueKey "manifests" value)))
          vect = valueKey "pkg:hackage/vector@0.13.2.0" (Aeson.Object resolved)
          errs = getValidationErrors (validateSnapshotValue value)

      KeyMap.size resolved `shouldBe` 3
      dependencies (entryFromValue vect) `shouldMatchList`
        [ "pkg:hackage/base@4.18.3.0"
        , "pkg:hackage/text@2.0.2"
        ]
      valueKey "relationship" vect `shouldBe` Aeson.String "direct"
      errs `shouldBe` []

prop_snapshotHasNoSelfPurlEdges :: SimplePlan -> Bool
prop_snapshotHasNoSelfPurlEdges (SimplePlan rawPlan) =
  all noSelfEdge (resolvedEntries snapshotValue)
 where
  snapshotValue =
    Aeson.toJSON (snapshotFromPlanGraph snapshotInput (extractPlanGraph rawPlan))

  noSelfEdge entry =
    packageUrl entry `notElem` dependencies entry

prop_snapshotHasUniqueDependencies :: SimplePlan -> Bool
prop_snapshotHasUniqueDependencies (SimplePlan rawPlan) =
  all unique (resolvedEntries snapshotValue)
 where
  snapshotValue =
    Aeson.toJSON (snapshotFromPlanGraph snapshotInput (extractPlanGraph rawPlan))

  unique entry =
    let deps = dependencies entry
     in length deps == Set.size (Set.fromList deps)

prop_snapshotHasClosure :: SimplePlan -> Bool
prop_snapshotHasClosure (SimplePlan rawPlan) =
  all depsKnown entries
 where
  snapshotValue =
    Aeson.toJSON (snapshotFromPlanGraph snapshotInput (extractPlanGraph rawPlan))

  resolved =
    expectObject (valueKey "resolved" (valueKey "cabal-project" (valueKey "manifests" snapshotValue)))

  resolvedKeys =
    Set.fromList (map Key.toText (KeyMap.keys resolved))

  entries =
    resolvedEntries snapshotValue

  depsKnown entry =
    all (`Set.member` resolvedKeys) (dependencies entry)

prop_directnessSoundness :: SimplePlan -> Bool
prop_directnessSoundness (SimplePlan rawPlan) =
  all sound directExternalPkgs
 where
  graph = extractPlanGraph rawPlan
  pkgs = Map.elems (planGraphPackages graph)

  localPkgs =
    filter ((== PackageLocal) . packageSource) pkgs

  directExternalPkgs =
    filter (\pkg -> packageSource pkg == PackageExternal && packageIsDirect pkg) pkgs

  sound pkg =
    any (\localPkg -> packageUnitId pkg `Set.member` packageDepends localPkg) localPkgs

data Entry = Entry
  { packageUrl :: Text.Text
  , dependencies :: [Text.Text]
  }

resolvedEntries :: Aeson.Value -> [Entry]
resolvedEntries value =
  [ entryFromValue v
  | (_, v) <- KeyMap.toList resolved
  ]
 where
  resolved =
    expectObject (valueKey "resolved" (valueKey "cabal-project" (valueKey "manifests" value)))

entryFromValue :: Aeson.Value -> Entry
entryFromValue =
  \case
    Aeson.Object o ->
      Entry
        { packageUrl =
            case KeyMap.lookup "package_url" o of
              Just (Aeson.String t) -> t
              _ -> error "missing package_url"
        , dependencies =
            case KeyMap.lookup "dependencies" o of
              Just (Aeson.Array arr) ->
                [ t | Aeson.String t <- toList arr ]
              _ ->
                error "missing dependencies"
        }
    _ ->
      error "expected resolved entry object"

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
    , rawPlanItemComponentName = Nothing
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
    , rawPlanItemPkgSrc = Nothing
    }

toList :: Foldable f => f a -> [a]
toList = foldr (:) []

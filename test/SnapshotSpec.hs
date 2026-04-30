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

    it "marks direct external dependencies soundly" $
      property prop_directnessSoundness

    it "collapses duplicate PURLs and still has no self edge" $ do
      let rawPlan =
            RawPlan
              { rawPlanCabalVersion = Nothing
              , rawPlanCompilerId = Nothing
              , rawPlanItems =
                  [ mkLocal "root-0.1.0.0-inplace" [UnitId "vector-A", UnitId "vector-B"]
                  , mkExternalWith "vector-A" "vector" "0.13.2.0" [UnitId "base-4.18.3.0", UnitId "vector-B"]
                  , mkExternalWith "vector-B" "vector" "0.13.2.0" [UnitId "base-4.18.3.0"]
                  , mkExternal "base-4.18.3.0" "base" "4.18.3.0"
                  ]
              }
          value = Aeson.toJSON (snapshotFromPlanGraph snapshotInput (extractPlanGraph rawPlan))
          resolved = expectObject (valueKey "resolved" (valueKey "cabal-project" (valueKey "manifests" value)))
          errs = getValidationErrors (validateSnapshotValue value)

      KeyMap.size resolved `shouldBe` 2
      errs `shouldBe` []

prop_snapshotHasNoSelfPurlEdges :: SimplePlan -> Bool
prop_snapshotHasNoSelfPurlEdges (SimplePlan rawPlan) =
  all noSelfEdge (resolvedEntries snapshotValue)
 where
  snapshotValue =
    Aeson.toJSON (snapshotFromPlanGraph snapshotInput (extractPlanGraph rawPlan))

  noSelfEdge entry =
    packageUrl entry `notElem` dependencies entry

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

toList :: Foldable f => f a -> [a]
toList = foldr (:) []

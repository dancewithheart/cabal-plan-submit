{-# LANGUAGE OverloadedStrings #-}

module SarifDeprecatedSpec (spec) where

import Data.Aeson (Value(..))
import Data.Aeson qualified as Aeson
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Vector qualified as Vector

import Hgs.Deprecated (Deprecation(..), findDeprecatedPackages)
import Hgs.Domain (PackageName(..))
import Hgs.Sarif.Deprecated (deprecatedPackagesSarif)
import Test.Hspec
import TestSupport (simpleGraph, valueKey)

spec :: Spec
spec =
  describe "deprecatedPackagesSarif" $ do
    it "emits one result per deprecated package" $ do
      length (sarifResults exampleSarif) `shouldBe` 2

    it "marks direct dependencies as warnings and transitive ones as notes" $ do
      fmap (valueKey "level") (resultFor aesonRule exampleSarif)
        `shouldBe` Just (String "warning")
      fmap (valueKey "level") (resultFor bytestringRule exampleSarif)
        `shouldBe` Just (String "note")

    it "includes replacement packages" $ do
      fmap (valueKey "cabal-plan-submit.replacements" . valueKey "properties")
        (resultFor aesonRule exampleSarif)
        `shouldBe` Just (Aeson.toJSON (["json-future"] :: [Text]))

    it "points to the indexed local dependency line" $ do
      (resultFor aesonRule exampleSarif >>= firstLocation)
        `shouldBe` Just (String "root.cabal", Aeson.toJSON (7 :: Int))

aesonRule :: Text
aesonRule = "haskell.deprecated-package.aeson.2.2.4.1"

bytestringRule :: Text
bytestringRule = "haskell.deprecated-package.bytestring.0.11.5.3"

exampleSarif :: Value
exampleSarif = deprecatedPackagesSarif lineIndex "." findings
 where
  lineIndex = Map.singleton ("root.cabal", PackageName "aeson") 7
  findings = findDeprecatedPackages deprecations simpleGraph

deprecations :: Map.Map PackageName Deprecation
deprecations =
  Map.fromList
    [ (PackageName "aeson", deprecation (PackageName "aeson") [PackageName "json-future"])
    , (PackageName "bytestring", deprecation (PackageName "bytestring") [])
    ]

deprecation :: PackageName -> [PackageName] -> Deprecation
deprecation name replacements =
  Deprecation
    { deprecationPackage = name
    , deprecationReplacements = replacements
    , deprecationReason = Nothing
    }

sarifResults :: Value -> [Value]
sarifResults sarif =
  case valueKey "runs" sarif of
    Array runs ->
      [ result
      | run <- Vector.toList runs
      , Array results <- [valueKey "results" run]
      , result <- Vector.toList results
      ]
    _ -> []

resultFor :: Text -> Value -> Maybe Value
resultFor ruleId = find ((== String ruleId) . valueKey "ruleId") . sarifResults

firstLocation :: Value -> Maybe (Value, Value)
firstLocation result =
  case valueKey "locations" result of
    Array locations ->
      case Vector.toList locations of
        [location] ->
          let physical = valueKey "physicalLocation" location
              artifact = valueKey "artifactLocation" physical
              region = valueKey "region" physical
           in Just ( valueKey "uri" artifact, valueKey "startLine" region )
        _ -> Nothing
    _ -> Nothing

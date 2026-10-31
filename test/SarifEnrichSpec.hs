{-# LANGUAGE OverloadedStrings #-}

module SarifEnrichSpec (spec) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Foldable (toList)
import Data.Text qualified as Text
import Hgs.Domain
  ( PlanGraph,
    RawPlan(..)
  , RawPkgSrc(..)
  , RawPlanItem(..)
  , UnitId(..)
  )
import Hgs.LocalUnitFilter (LocalUnitFilter(..))
import Hgs.Sarif.Enrich (enrichSarifValue)
import Hgs.Sarif.Common (locationValue)
import Test.Hspec
import TestSupport
  ( graphFromValidPlan
  , mkExternal
  , mkExternalWith
  , mkLocal
  , valueKey
  )

spec :: Spec
spec = do
  describe "enrichSarifValue" $ do
    it "omits a region when the dependency line is unknown" $ do
      let physical = objectField "physicalLocation" (expectLocation (locationValue "root.cabal" Nothing))
      KeyMap.lookup "region" physical `shouldBe` Nothing

    it "preserves an unmatched finding's original location" $ do
      let root = (mkLocal "root" []) { rawPlanItemPkgSrc = Just (RawPkgSrc (Just "local") (Just "/repo")) }
          rootGraph = graphFromValidPlan (RawPlan Nothing Nothing Nothing Nothing [root])
          original = sarifResult "UNRESOLVED" "unrecognized advisory"
          enriched = enrichSarifValue mempty AllLocalUnits rootGraph original
      firstResult enriched `shouldBe` firstResult original

    it "does not invent a cabal file when none was found" $ do
      let enriched = enrichSarifValue mempty AllLocalUnits graph cryptoniteSarif
      KeyMap.lookup "locations" (firstResult enriched)
        `shouldBe` KeyMap.lookup "locations" (firstResult cryptoniteSarif)
    it "enriches cabal-audit result with dependency path" $ do
      let value =
            enrichSarifValue mempty AllLocalUnits graph cryptoniteSarif

          result =
            firstResult value

          messageText =
            stringField "text" (objectField "message" result)

          properties =
            objectField "properties" result

      messageText `shouldContainText` "cabal-plan-submit:"
      messageText `shouldContainText` "root-0.1.0.0 -> cryptohash-0.11.9 -> cryptonite-0.30"

      stringField "cabal-plan-submit.package" properties
        `shouldBe` "cryptonite-0.30"

      stringField "cabal-plan-submit.relationship" properties
        `shouldBe` "indirect"

    it "does not confuse crypton with cryptonite" $ do
      let value =
            enrichSarifValue mempty AllLocalUnits graph cryptonSarif

          result =
            firstResult value

          messageText =
            stringField "text" (objectField "message" result)

      messageText `shouldContainText` "crypton-1.1.2"
      messageText `shouldNotContainText` "cryptonite-0.30"

    it "does not guess a package version from a substring in advisory text" $ do
      let multiVersionGraph = graphFromValidPlan $ RawPlan Nothing Nothing Nothing Nothing
            [ mkLocal "root" [UnitId "process-a", UnitId "process-b"]
            , mkExternal "process-a" "process" "1.6.1"
            , mkExternal "process-b" "process" "1.6.10"
            ]
          input = sarifResult "TEST-VERSIONS" "mentioned version 1.6.10\n(process)\n"
      firstResult (enrichSarifValue mempty AllLocalUnits multiVersionGraph input)
        `shouldBe` firstResult input

    it "does not treat unrelated result metadata as an advisory package" $ do
      let input = Aeson.object
            [ "runs" Aeson..= [Aeson.object
              [ "results" Aeson..= [Aeson.object
                [ "ruleId" Aeson..= ("TEST-METADATA" :: Text.Text)
                , "message" Aeson..= Aeson.object ["text" Aeson..= ("unrecognized advisory" :: Text.Text)]
                , "properties" Aeson..= Aeson.object ["note" Aeson..= ("\n(crypton)\n" :: Text.Text)]
                ]]
              ]]
            ]
      firstResult (enrichSarifValue mempty AllLocalUnits graph input) `shouldBe` firstResult input

    it "treats multiple units of one package version as one package" $ do
      let multiUnitGraph = graphFromValidPlan $ RawPlan Nothing Nothing Nothing Nothing
            [ mkLocal "root" [UnitId "shared-lib", UnitId "shared-exe"]
            , mkExternal "shared-lib" "shared" "1.2.3"
            , mkExternal "shared-exe" "shared" "1.2.3"
            ]
          input = sarifResult "TEST-UNITS" "finding\n(shared)\n"
          result = firstResult (enrichSarifValue mempty AllLocalUnits multiUnitGraph input)
      stringField "cabal-plan-submit.package" (objectField "properties" result)
        `shouldBe` "shared-1.2.3"

    it "preserves the original severity for indirect findings" $ do
      let value = enrichSarifValue mempty AllLocalUnits graph cryptoniteSarif
          result = firstResult value
      stringField "ruleId" result `shouldBe` "HSEC-2025-0002"
      stringField "level" result `shouldBe` "error"

    it "preserves the original severity for direct findings" $ do
      let input = sarifResultWithLevel (Just "warning") "TEST-CRYPTON" "Some finding\n(crypton)\ncrypto"
          value = enrichSarifValue mempty AllLocalUnits graph input
          result = firstResult value
      stringField "level" result `shouldBe` "warning"

    it "does not invent a result-level severity when it was absent" $ do
      let input = sarifResultWithLevel Nothing "TEST-CRYPTON" "Some finding\n(crypton)\ncrypto"
          value = enrichSarifValue mempty AllLocalUnits graph input
      KeyMap.lookup "level" (firstResult value) `shouldBe` Nothing

    it "preserves the producer's precision, severity and tags" $ do
      let originalProperties = objectField "properties" (firstResult cryptoniteSarif)
          enrichedProperties = objectField "properties" (firstResult (enrichSarifValue mempty AllLocalUnits graph cryptoniteSarif))
      map (`KeyMap.lookup` enrichedProperties) ["precision", "problem.severity", "security-severity", "tags"]
        `shouldBe` map (`KeyMap.lookup` originalProperties) ["precision", "problem.severity", "security-severity", "tags"]

    it "does not create markdown that hides an original text-only advisory" $ do
      let result = firstResult (enrichSarifValue mempty AllLocalUnits graph cryptoniteSarif)
          message = objectField "message" result
      KeyMap.lookup "markdown" message `shouldBe` Nothing
      stringField "text" message `shouldContainText` "Double Public Key Signing"

graph :: Hgs.Domain.PlanGraph
graph =
  graphFromValidPlan $
    RawPlan
      { rawPlanCabalVersion = Nothing
      , rawPlanCompilerId = Nothing
      , rawPlanOS = Nothing
      , rawPlanArch = Nothing
      , rawPlanItems =
          [ mkLocal
              "root-0.1.0.0-inplace"
              [ UnitId "cryptohash-0.11.9"
              , UnitId "crypton-1.1.2"
              ]
          , mkExternalWith
              "cryptohash-0.11.9"
              "cryptohash"
              "0.11.9"
              [UnitId "cryptonite-0.30"]
          , mkExternal "cryptonite-0.30" "cryptonite" "0.30"
          , mkExternal "crypton-1.1.2" "crypton" "1.1.2"
          ]
      }

cryptoniteSarif :: Aeson.Value
cryptoniteSarif =
  sarifResult
    "HSEC-2025-0002"
    "Double Public Key Signing Function Oracle Attack on Ed25519\n(cryptonite)\ncrypto"

cryptonSarif :: Aeson.Value
cryptonSarif =
  sarifResult
    "TEST-CRYPTON"
    "Some finding\n(crypton)\ncrypto"

sarifResult :: Text.Text -> Text.Text -> Aeson.Value
sarifResult = sarifResultWithLevel (Just "error")

sarifResultWithLevel :: Maybe Text.Text -> Text.Text -> Text.Text -> Aeson.Value
sarifResultWithLevel level ruleId messageText =
  Aeson.object
    [ "version" Aeson..= ("2.1.0" :: Text.Text)
    , "runs" Aeson..=
        [ Aeson.object
            [ "results" Aeson..=
                [ Aeson.object $
                    [ "ruleId" Aeson..= ruleId
                    , "message" Aeson..=
                        Aeson.object
                          [ "text" Aeson..= messageText
                          ]
                    , "locations" Aeson..=
                        [ Aeson.object
                            [ "physicalLocation" Aeson..=
                                Aeson.object
                                  [ "artifactLocation" Aeson..=
                                      Aeson.object
                                        [ "uri" Aeson..= ("file:///repo" :: Text.Text)
                                        ]
                                  ]
                            ]
                        ]
                    , "properties" Aeson..= Aeson.object
                        [ "precision" Aeson..= ("high" :: Text.Text)
                        , "problem.severity" Aeson..= ("warning" :: Text.Text)
                        , "security-severity" Aeson..= ("9.1" :: Text.Text)
                        , "tags" Aeson..= (["source-tag"] :: [Text.Text])
                        ]
                    ] <> maybe [] (\value -> ["level" Aeson..= value]) level
                ]
            ]
        ]
    ]

firstResult :: Aeson.Value -> KeyMap.KeyMap Aeson.Value
firstResult value =
  case valueKey "runs" value of
    Aeson.Array runs ->
      case toList runs of
        Aeson.Object run : _ ->
          case KeyMap.lookup "results" run of
            Just (Aeson.Array results) ->
              case toList results of
                Aeson.Object result : _ ->
                  result
                _ ->
                  error "expected first result object"
            _ ->
              error "expected results array"
        _ ->
          error "expected first run object"
    _ ->
      error "expected runs array"

objectField :: Text.Text -> KeyMap.KeyMap Aeson.Value -> KeyMap.KeyMap Aeson.Value
objectField key object =
  case KeyMap.lookup (Key.fromText key) object of
    Just (Aeson.Object value) ->
      value
    _ ->
      error ("expected object field: " <> Text.unpack key)

stringField :: Text.Text -> KeyMap.KeyMap Aeson.Value -> Text.Text
stringField key object =
  case KeyMap.lookup (Key.fromText key) object of
    Just (Aeson.String value) ->
      value
    _ ->
      error ("expected string field: " <> Text.unpack key)

shouldContainText :: Text.Text -> Text.Text -> Expectation
shouldContainText actual expected =
  Text.unpack actual `shouldContain` Text.unpack expected

shouldNotContainText :: Text.Text -> Text.Text -> Expectation
shouldNotContainText actual expected =
  Text.unpack actual `shouldNotContain` Text.unpack expected

expectLocation :: Aeson.Value -> KeyMap.KeyMap Aeson.Value
expectLocation (Aeson.Object location) = location
expectLocation _ = error "expected location object"

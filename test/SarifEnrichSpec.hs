{-# LANGUAGE OverloadedStrings #-}

module SarifEnrichSpec (spec) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Foldable (toList)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Hgs.Domain
  ( PlanGraph,
    RawPlan(..)
  , UnitId(..)
  )
import Hgs.Extract (extractPlanGraph)
import Hgs.LocalUnitFilter (LocalUnitFilter(..))
import Hgs.Sarif.Enrich (enrichSarifValue)
import Test.Hspec
import TestSupport
  ( mkExternal
  , mkExternalWith
  , mkLocal
  , valueKey
  )

spec :: Spec
spec = do
  describe "enrichSarifValue" $ do
    it "enriches cabal-audit result with dependency path" $ do
      let value =
            enrichSarifValue Map.empty AllLocalUnits graph cryptoniteSarif

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
            enrichSarifValue Map.empty AllLocalUnits graph cryptonSarif

          result =
            firstResult value

          messageText =
            stringField "text" (objectField "message" result)

      messageText `shouldContainText` "crypton-1.1.2"
      messageText `shouldNotContainText` "cryptonite-0.30"

    it "preserves rule id while setting effective SARIF level" $ do
      let value = enrichSarifValue Map.empty AllLocalUnits graph cryptoniteSarif
          result = firstResult value
      stringField "ruleId" result `shouldBe` "HSEC-2025-0002"
      stringField "level" result `shouldBe` "note"

    it "marks direct dependency findings as error" $ do
      let value = enrichSarifValue Map.empty AllLocalUnits graph cryptonSarif
          result = firstResult value
      stringField "level" result `shouldBe` "error"

graph :: Hgs.Domain.PlanGraph
graph =
  extractPlanGraph $
    RawPlan
      { rawPlanCabalVersion = Nothing
      , rawPlanCompilerId = Nothing
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
sarifResult ruleId messageText =
  Aeson.object
    [ "version" Aeson..= ("2.1.0" :: Text.Text)
    , "runs" Aeson..=
        [ Aeson.object
            [ "results" Aeson..=
                [ Aeson.object
                    [ "ruleId" Aeson..= ruleId
                    , "level" Aeson..= ("error" :: Text.Text)
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
                    ]
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

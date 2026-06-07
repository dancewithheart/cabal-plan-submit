{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module GoldenSpec (spec) where

import Data.Aeson (Value(..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import System.Environment (lookupEnv)
import Hgs.Deprecated
  ( findDeprecatedPackagesFrom
  , readDeprecationIndex
  , renderDeprecatedPackages
  , Deprecation
  )
import Hgs.Domain (PackageName, RawPlan)
import Hgs.Extract (extractPlanGraph)
import Hgs.Input.PlanJson (decodeRawPlan)
import Hgs.LocalUnitFilter (LocalUnitFilter(..))
import Hgs.Sarif.Enrich
  ( enrichSarifValue
  )
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  )
import System.FilePath
  ( takeDirectory
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "golden use cases" $ do
    golden
      "inspect-deprecated --production-only reports old-time in unix-time"
      "test/golden/unix-time/inspect-deprecated-production.golden"
      (renderInspectDeprecatedProductionOnly "test/golden/unix-time/plan.json" "test/golden/deprecated.yaml")

    golden
      "enrich-sarif adds dependency path projection for persistent cabal-audit SARIF"
      "test/golden/persistent/enrich-sarif-projection.golden"
      (renderEnrichedSarifProjection "test/golden/persistent/plan.json" "test/golden/persistent/sarif.json")

golden :: String -> FilePath -> IO ByteString -> Spec
golden name expectedPath action =
  it name $ do
    actual <- action
    update <- shouldUpdateGolden

    if update
      then do
        createDirectoryIfMissing True (takeDirectory expectedPath)
        BS.writeFile expectedPath actual
      else do
        exists <- doesFileExist expectedPath
        if not exists
          then expectationFailure $
            unlines
              [ "missing golden file: " <> expectedPath
              , "Run with UPDATE_GOLDEN=1 to create it."
              ]
          else do
            expected <- BS.readFile expectedPath
            actual `shouldBe` expected

shouldUpdateGolden :: IO Bool
shouldUpdateGolden = do
  value <- lookupEnv "UPDATE_GOLDEN"
  pure (value `elem` [Just "1", Just "true", Just "yes"])

renderInspectDeprecatedProductionOnly :: FilePath -> FilePath -> IO ByteString
renderInspectDeprecatedProductionOnly planPath deprecatedPath = do
  plan <- decodePlanFileOrFail planPath
  index <- readDeprecatedIndexOrFail deprecatedPath

  let graph =
        extractPlanGraph plan

      deprecated =
        findDeprecatedPackagesFrom ProductionLocalUnits index graph

  pure (BS8.pack (renderDeprecatedPackages deprecated))

renderEnrichedSarifProjection :: FilePath -> FilePath -> IO ByteString
renderEnrichedSarifProjection planPath sarifPath = do
  plan <- decodePlanFileOrFail planPath
  sarif <- decodeJsonFileOrFail sarifPath

  let enriched =
        enrichSarifValue
          Map.empty
          AllLocalUnits
          (extractPlanGraph plan)
          sarif

  pure (BS8.pack (renderSarifProjection enriched))

decodePlanFileOrFail :: FilePath -> IO RawPlan
decodePlanFileOrFail path = do
  bytes <- readFixtureOrFail path
  case decodeRawPlan bytes of
    Left err ->
      expectationFailure ("failed to decode plan fixture " <> path <> ": " <> err) *> fail "unreachable"
    Right plan ->
      pure plan

-- readDeprecatedIndexOrFail :: FilePath -> IO a
readDeprecatedIndexOrFail :: FilePath
  -> IO (Map.Map PackageName Deprecation)
readDeprecatedIndexOrFail path = do
  result <- readDeprecationIndex path
  case result of
    Left err ->
      expectationFailure ("failed to decode deprecated fixture " <> path <> ": " <> err) *> fail "unreachable"

    Right index ->
      pure index

decodeJsonFileOrFail :: FilePath -> IO Value
decodeJsonFileOrFail path = do
  bytes <- readFixtureOrFail path
  case Aeson.eitherDecodeStrict' bytes of
    Left err ->
      expectationFailure ("failed to decode JSON fixture " <> path <> ": " <> err) *> fail "unreachable"
    Right value ->
      pure value

renderSarifProjection :: Value -> String
renderSarifProjection value =
  case sarifResults value of
    [] ->
      "no enriched results\n"

    results ->
      concatMap renderResultProjection results

renderResultProjection :: KeyMap.KeyMap Value -> String
renderResultProjection result =
  case cabalPlanSubmitProperties result of
    Nothing ->
      ""

    Just props ->
      unlines $
        [ valueOrMissing "ruleId" result
        , "  level: " <> valueOrMissing "level" result
        , "  package: " <> valueOrMissing "cabal-plan-submit.package" props
        , "  relationship: " <> valueOrMissing "cabal-plan-submit.relationship" props
        , "  paths:"
        ]
          <> [ "    - " <> Text.unpack path
             | path <- stringArrayField "cabal-plan-submit.paths" props
             ]
          <> [ "" ]

cabalPlanSubmitProperties :: KeyMap.KeyMap Value -> Maybe (KeyMap.KeyMap Value)
cabalPlanSubmitProperties result = do
  Object props <- KeyMap.lookup "properties" result
  _ <- KeyMap.lookup "cabal-plan-submit.package" props
  pure props

sarifResults :: Value -> [KeyMap.KeyMap Value]
sarifResults =
  \case
    Object root ->
      case KeyMap.lookup "runs" root of
        Just (Array runs) ->
          concatMap runResults (Vector.toList runs)

        _ ->
          []

    _ ->
      []

runResults :: Value -> [KeyMap.KeyMap Value]
runResults =
  \case
    Object run ->
      case KeyMap.lookup "results" run of
        Just (Array results) ->
          [ result
          | Object result <- Vector.toList results
          ]

        _ ->
          []

    _ ->
      []

valueOrMissing :: Text.Text -> KeyMap.KeyMap Value -> String
valueOrMissing key object =
  case KeyMap.lookup (Key.fromText key) object of
    Just (String value) ->
      Text.unpack value

    Just value ->
      show value

    Nothing ->
      "<missing>"

stringArrayField :: Text.Text -> KeyMap.KeyMap Value -> [Text.Text]
stringArrayField key object =
  case KeyMap.lookup (Key.fromText key) object of
    Just (Array values) ->
      [ value
      | String value <- Vector.toList values
      ]

    _ ->
      []

readFixtureOrFail :: FilePath -> IO ByteString
readFixtureOrFail path = do
  exists <- doesFileExist path
  if exists
    then BS.readFile path
    else expectationFailure
      ( unlines
          [ "missing fixture file: " <> path
          , ""
          , "Create it before running golden tests."
          , "Example:"
          , "  mkdir -p test/golden/persistent"
          , "  cp ./golden/persistent_plan.json test/golden/persistent/plan.json"
          ]
      ) *> fail "unreachable"

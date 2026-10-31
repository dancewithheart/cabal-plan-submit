{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module GoldenSpec (spec) where

import Control.Monad (unless)
import Data.Set qualified as Set
import Data.Aeson (Value(..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
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
import Hgs.Snapshot
  ( snapshotFromPlanGraph
  )
import TestSupport
  ( graphFromValidPlan
  , snapshotInput
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "SARIF result preservation" $ do
    it "reports both packages in the real HSEC-2026-0007 result" $ do
      plan <- decodePlanFileOrFail "test/golden/persistent/plan.json"
      sarif <- decodeJsonFileOrFail "test/golden/persistent/sarif.json"
      let enriched = enrichSarifValue mempty AllLocalUnits (graphFromValidPlan plan) sarif
          findings = filter ((== "HSEC-2026-0007") . valueOrMissing "ruleId") (sarifResults enriched)
      case findings of
        [finding] -> do
          let properties = expectProps finding
          KeyMap.lookup "cabal-plan-submit.match" properties `shouldBe` Just (String "multiple-packages")
          case KeyMap.lookup "cabal-plan-submit.packages" properties of
            Just (Array packages) -> do
              let entries = [entry | Object entry <- Vector.toList packages]
              map (\entry -> (valueOrMissing "name" entry, valueOrMissing "version" entry)) entries
                `shouldMatchList` [("aeson", "2.2.5.0"), ("text-iso8601", "0.1.1.1")]
              map (valueOrMissing "match") entries `shouldSatisfy` all (== "matched")
              map (valueOrMissing "relationship") entries `shouldSatisfy` all (`elem` ["direct", "indirect"])
            _ -> expectationFailure "missing package matches"
        _ -> expectationFailure "expected exactly one preserved advisory"

    it "keeps unmatched advisories in the full enriched SARIF" $ do
      plan <- decodePlanFileOrFail "test/golden/persistent/plan.json"
      sarif <- decodeJsonFileOrFail "test/golden/persistent/sarif.json"
      let enriched = enrichSarifValue mempty AllLocalUnits (graphFromValidPlan plan) sarif
          ruleIds = map (valueOrMissing "ruleId") . sarifResults
      ruleIds enriched `shouldBe` ruleIds sarif
      ruleIds enriched `shouldContain` ["HSEC-2026-0007"]

  describe "golden use cases" $ do
    golden
      "inspect-deprecated --production-only reports old-time in unix-time"
      "test/golden/unix-time/inspect-deprecated-production.golden"
      (renderInspectDeprecatedProductionOnly "test/golden/unix-time/plan.json" "test/golden/deprecated.yaml")

    golden
      "enrich-sarif adds dependency path projection for persistent cabal-audit SARIF"
      "test/golden/persistent/enrich-sarif-projection.golden"
      (renderEnrichedSarifProjection "test/golden/persistent/plan.json" "test/golden/persistent/sarif.json")

    golden
      "enrich-sarif renders full enriched persistent SARIF"
      "test/golden/persistent/sarif_enriched.json"
      (renderEnrichedSarif "test/golden/persistent/plan.json" "test/golden/persistent/sarif.json")

    golden
      "render-snapshot renders GitHub dependency snapshot"
      "test/golden/cabal_plan_json/snapshot.json"
      (renderSnapshot "test/golden/cabal_plan_json/plan.json")

expectProps :: KeyMap.KeyMap Value -> KeyMap.KeyMap Value
expectProps finding = case KeyMap.lookup "properties" finding of
  Just (Object props) -> props
  _ -> error "expected finding properties"

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
            -- actual `shouldBe` expected -- very verbose error
            unless (actual == expected) $
              expectationFailure $
                "Golden mismatch: " <> expectedPath
                  <> "\nFirst differences (up to 12):\n"
                  <> unlines (take 12 (goldenDiff expected actual))

shouldUpdateGolden :: IO Bool
shouldUpdateGolden = do
  value <- lookupEnv "UPDATE_GOLDEN"
  pure (value `elem` [Just "1", Just "true", Just "yes"])

renderInspectDeprecatedProductionOnly :: FilePath -> FilePath -> IO ByteString
renderInspectDeprecatedProductionOnly planPath deprecatedPath = do
  plan <- decodePlanFileOrFail planPath
  index <- readDeprecatedIndexOrFail deprecatedPath

  let graph =
        graphFromValidPlan plan

      deprecated =
        findDeprecatedPackagesFrom ProductionLocalUnits index graph

  pure (BS8.pack (renderDeprecatedPackages deprecated))

renderEnrichedSarifProjection :: FilePath -> FilePath -> IO ByteString
renderEnrichedSarifProjection planPath sarifPath = do
  plan <- decodePlanFileOrFail planPath
  sarif <- decodeJsonFileOrFail sarifPath

  let enriched =
        enrichSarifValue
          mempty
          AllLocalUnits
          (graphFromValidPlan plan)
          sarif

  pure (BS8.pack (renderSarifProjection enriched))

renderEnrichedSarif :: FilePath -> FilePath -> IO ByteString
renderEnrichedSarif planPath sarifPath = do
  plan <- decodePlanFileOrFail planPath
  sarif <- decodeJsonFileOrFail sarifPath

  let enriched =
        enrichSarifValue
          mempty
          AllLocalUnits
          (graphFromValidPlan plan)
          sarif

  pure (encodeJsonStrict enriched)

renderSnapshot :: FilePath -> IO ByteString
renderSnapshot planPath = do
  plan <- decodePlanFileOrFail planPath

  let graph =
        graphFromValidPlan plan

      snapshot =
        snapshotFromPlanGraph snapshotInput graph

  pure (encodeJsonStrict snapshot)

encodeJsonStrict :: Aeson.ToJSON a => a -> ByteString
encodeJsonStrict value =
  LBS.toStrict (Aeson.encode value) <> "\n"

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

goldenDiff :: ByteString -> ByteString -> [String]
goldenDiff expected actual =
  case (Aeson.decodeStrict' expected, Aeson.decodeStrict' actual) of
    (Just e, Just a)
      | e == a -> ["JSON values match; formatting differs"]
      | otherwise -> jsonDiff "$" e a
    _ ->
      let beforeLines = BS8.lines expected
          afterLines = BS8.lines actual
          n = length (takeWhile (uncurry (==)) (zip beforeLines afterLines))
          at xs = case drop n xs of
            x : _ -> show (BS8.take 160 x)
            [] -> "<end of file>"
       in [ "First different line: " <> show (n + 1)
          , "- " <> at beforeLines
          , "+ " <> at afterLines
          ]

jsonDiff :: String -> Value -> Value -> [String]
jsonDiff _path expected actual
  | expected == actual = []

jsonDiff path (Object expected) (Object actual) =
  concatMap diffKey keys
 where
  keys =
    Set.toAscList $
      Set.fromList (KeyMap.keys expected <> KeyMap.keys actual)

  diffKey key =
    let child = path <> "." <> Key.toString key
     in case (KeyMap.lookup key expected, KeyMap.lookup key actual) of
          (Just e, Just a) -> jsonDiff child e a
          (Just _, Nothing) -> ["- " <> child]
          (Nothing, Just _) -> ["+ " <> child]
          _ -> []

jsonDiff path (Array expected) (Array actual) =
  case (traverse asText (Vector.toList expected),
        traverse asText (Vector.toList actual)) of
    (Just es, Just as) ->
      let removed = ["- " <> path <> ": " <> Text.unpack x
                    | x <- es, x `notElem` as]
          added = ["+ " <> path <> ": " <> Text.unpack x
                  | x <- as, x `notElem` es]
       in if null removed && null added
            then [path <> ": order or multiplicities changed"]
            else removed <> added

    _ -> [path <> ": array changed"]

jsonDiff path expected actual =
  [ path <> ": "
      <> take 100 (show expected)
      <> " -> "
      <> take 100 (show actual)
  ]

asText :: Value -> Maybe Text.Text
asText (String t) = Just t
asText _ = Nothing

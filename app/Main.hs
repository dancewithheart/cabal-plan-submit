{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (IOException, try)
import Control.Monad (when)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.List (isSuffixOf, sort)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time.Clock (getCurrentTime)
import Data.Version (showVersion)
import Paths_cabal_plan_submit qualified as Paths
import System.Directory (doesFileExist, listDirectory)
import System.Environment (getArgs)
import System.Exit (die, exitFailure)
import System.IO (hPutStrLn, stderr)

import Hgs.Deprecated
  ( FailOnDeprecated(..)
  , filterDeprecatedPackagesIgnoring
  , findDeprecatedPackagesFrom
  , readDeprecationIndex
  , renderDeprecatedPackages
  , shouldFailOnDeprecated
  )
import Hgs.Domain
  ( PackageName(..)
  , PlanGraph(..)
  , RawPlan
  )
import Hgs.Extract (extractPlanGraphChecked, summarisePlanGraph)
import Hgs.Input.PlanJson (readRawPlan, summariseRawPlan)
import Hgs.Locals
  ( inspectLocals
  , renderLocals
  )
import Hgs.Snapshot
  ( SnapshotInput(..)
  , encodeSnapshot
  , snapshotFromPlanGraph
  )
import Hgs.Validate
  ( isValid
  , renderValidationReport
  , validateSnapshotFile
  )
import Hgs.Why (renderWhyFrom, renderWhyTrieFrom)
import Hgs.LocalUnitFilter
  ( LocalUnitFilter(..)
  )
import Hgs.Sarif.Deprecated
  ( deprecatedPackagesSarif
  )
import Hgs.CabalFileIndex
  ( buildCabalLineIndex
  , guessRepoRoot
  )
import Hgs.Sarif.Enrich (enrichSarifValue)

main :: IO ()
main = do
  rawArgs <- getArgs
  let args = normalizeExternalCommandArgs rawArgs
  runWithArgs rawArgs args

runWithArgs :: [String] -> [String] -> IO ()
runWithArgs rawArgs args =
  case args of
    ["--help"] ->
      putStr usage
    ["-h"] ->
      putStr usage
    ["--version"] ->
      putStrLn ("cabal-plan-submit " <> showVersion Paths.version)
    ["inspect-plan"] ->
      inspectPlan defaultPlanJsonPath
    ["inspect-plan", path] ->
      inspectPlan path
    ["inspect-graph"] ->
      inspectGraph defaultPlanJsonPath
    ["inspect-graph", path] ->
      inspectGraph path
    ["inspect-locals"] ->
      inspectLocalPackages defaultPlanJsonPath
    ["inspect-locals", path] ->
      inspectLocalPackages path
    ["render-snapshot", path, sha, ref] ->
      renderSnapshot path sha ref
    ["validate-snapshot", path] ->
      validateSnapshot path
    -- TODO pp drop this in favour of deprecated, but I have working GH workflows using this so not now
    "inspect-deprecated" : rest ->
      case parseInspectDeprecatedOptions rest of
        Left err   -> dieWithArgs rawArgs err
        Right opts -> inspectDeprecated opts
    "deprecated" : rest ->
      case parseInspectDeprecatedOptions rest of
        Left err   -> die err
        Right opts -> inspectDeprecated opts
    ["why", "--production-only", packageName] ->
      whyPackage ProductionLocalUnits defaultPlanJsonPath packageName
    ["why", packageName] ->
      whyPackage AllLocalUnits defaultPlanJsonPath packageName
    ["why", "--production-only", path, packageName] ->
      whyPackage ProductionLocalUnits path packageName
    ["why", path, packageName] ->
      whyPackage AllLocalUnits path packageName
    ["enrich-sarif", sarifPath] ->
      enrichSarif AllLocalUnits defaultPlanJsonPath sarifPath
    ["why-tree", path, packageName] ->
      whyPackageTree AllLocalUnits path packageName
    ["why-tree", "--production-only", path, packageName] ->
      whyPackageTree ProductionLocalUnits path packageName
    ["why-tree", packageName] ->
      whyPackageTree AllLocalUnits defaultPlanJsonPath packageName
    ["why-tree", "--production-only", packageName] ->
      whyPackageTree ProductionLocalUnits defaultPlanJsonPath packageName
    ["enrich-sarif", planPath, sarifPath] ->
      enrichSarif AllLocalUnits planPath sarifPath
    ["enrich-sarif", "--production-only", sarifPath] ->
      enrichSarif ProductionLocalUnits defaultPlanJsonPath sarifPath
    ["enrich-sarif", "--production-only", planPath, sarifPath] ->
      enrichSarif ProductionLocalUnits planPath sarifPath
    "deprecated-sarif" : rest ->
      case parseDeprecatedSarifOptions rest of
        Left err ->
          dieWithArgs rawArgs err
        Right opts ->
          deprecatedSarif
            (deprecatedSarifLocalFilter opts)
            (deprecatedSarifIgnoredPackages opts)
            (deprecatedSarifPlanPath opts)
            (deprecatedSarifMetadataPath opts)
    _ -> dieWithArgs rawArgs usage

defaultPlanJsonPath :: FilePath
defaultPlanJsonPath = "dist-newstyle/cache/plan.json"

defaultDeprecatedYamlPath :: FilePath
defaultDeprecatedYamlPath = "deprecated.yaml"

-- cabal-install 3.14 passes the external command name as argv[1]:
--   cabal plan-submit why aeson
-- becomes:
--   cabal-plan-submit plan-submit why aeson
normalizeExternalCommandArgs :: [String] -> [String]
normalizeExternalCommandArgs =
  \case
    "plan-submit" : rest -> rest
    args -> args

dieWithArgs :: [String] -> String -> IO a
dieWithArgs rawArgs msg =
  die $
    unlines
      [ "Invalid command or arguments."
      , ""
      , "Received arguments:"
      , "  " <> show rawArgs
      , ""
      , msg
      ]

data InspectDeprecatedOptions = InspectDeprecatedOptions
  { inspectDeprecatedLocalFilter     :: LocalUnitFilter
  , inspectDeprecatedFailOn          :: FailOnDeprecated
  , inspectDeprecatedIgnoredPackages :: Set PackageName
  , inspectDeprecatedPlanPath        :: FilePath
  , inspectDeprecatedMetadataPath    :: FilePath
  }
  deriving stock (Eq, Show)

defaultInspectDeprecatedOptions :: InspectDeprecatedOptions
defaultInspectDeprecatedOptions =
  InspectDeprecatedOptions
    { inspectDeprecatedLocalFilter = AllLocalUnits
    , inspectDeprecatedFailOn = FailOnNone
    , inspectDeprecatedIgnoredPackages = Set.empty
    , inspectDeprecatedPlanPath = ""
    , inspectDeprecatedMetadataPath = ""
    }

parseInspectDeprecatedOptions :: [String] -> Either String InspectDeprecatedOptions
parseInspectDeprecatedOptions =
  go defaultInspectDeprecatedOptions
 where
  go opts =
    \case
      "--production-only" : rest ->
        go opts { inspectDeprecatedLocalFilter = ProductionLocalUnits } rest

      "--fail-on" : failOn : rest ->
        case parseFailOnDeprecated failOn of
          Nothing ->
            Left ("unknown --fail-on value: " <> failOn <> "\nExpected one of: none, direct, any")
          Just policy ->
            go opts { inspectDeprecatedFailOn = policy } rest

      "--ignore-package" : pkgName : rest ->
        go
          opts
            { inspectDeprecatedIgnoredPackages =
                Set.insert
                  (PackageName (Text.pack pkgName))
                  (inspectDeprecatedIgnoredPackages opts)
            }
          rest

      [] ->
        Right
          opts
            { inspectDeprecatedPlanPath = defaultPlanJsonPath
            , inspectDeprecatedMetadataPath = defaultDeprecatedYamlPath
            }
      [metadataPath] ->
        Right
          opts
            { inspectDeprecatedPlanPath = defaultPlanJsonPath
            , inspectDeprecatedMetadataPath = metadataPath
            }
      [planPath, metadataPath] ->
        Right
          opts
            { inspectDeprecatedPlanPath = planPath
            , inspectDeprecatedMetadataPath = metadataPath
            }

      _ ->
        Left usage

inspectPlan :: FilePath -> IO ()
inspectPlan path = do
  plan <- readPlanOrDie path
  putStrLn (summariseRawPlan plan)

inspectGraph :: FilePath -> IO ()
inspectGraph path = do
  graph <- readGraphOrDie path
  putStrLn (summarisePlanGraph graph)

renderSnapshot :: FilePath -> String -> String -> IO ()
renderSnapshot path sha ref = do
  graph <- readGraphOrDie path
  scannedAt <- getCurrentTime
  manifestPath <- detectManifestPath
  let input =
        SnapshotInput
          { snapshotSha = Text.pack sha
          , snapshotRef = Text.pack ref
          , snapshotScannedAt = scannedAt
          , snapshotJobId = "github-actions"
          , snapshotCorrelator = "dependency-submission"
          , snapshotManifestKey = "cabal-project"
          , snapshotManifestName = "cabal project"
          , snapshotManifestPath = manifestPath
          , snapshotDetectorName = "cabal-plan-submit"
          , snapshotDetectorVersion = Text.pack (showVersion Paths.version)
          , snapshotDetectorUrl = "https://github.com/dancewithheart/cabal-plan-submit"
          }
      snapshot =
        snapshotFromPlanGraph input graph
  LBS8.putStrLn (encodeSnapshot snapshot)

validateSnapshot :: FilePath -> IO ()
validateSnapshot path = do
  result <- validateSnapshotFile path
  case result of
    Left err ->
      die ("failed to parse snapshot.json: " <> err)
    Right errs
      | isValid errs ->
          putStrLn (renderValidationReport errs)
      | otherwise -> do
          putStrLn (renderValidationReport errs)
          exitFailure

readPlanOrDie :: FilePath -> IO RawPlan
readPlanOrDie path = do
  exists <- doesFileExist path
  if not exists
    then die (missingPlanMessage path)
    else do
      ePlan <- readRawPlan path
      case ePlan of
        Left err ->
          die ("failed to parse plan.json: " <> err)
        Right plan ->
          pure plan

readGraphOrDie :: FilePath -> IO PlanGraph
readGraphOrDie path = do
  plan <- readPlanOrDie path
  either (die . ("invalid plan graph: " <>) . show) pure (extractPlanGraphChecked plan)

detectManifestPath :: IO (Maybe FilePath)
detectManifestPath = do
  cabalProjectExists <- doesFileExist "cabal.project"
  if cabalProjectExists
    then pure (Just "cabal.project")
    else do
      files <- listDirectory "."
      pure $
        case sort (filter (isSuffixOf ".cabal") files) of
          cabalFile : _ -> Just cabalFile
          [] -> Nothing

missingPlanMessage :: FilePath -> String
missingPlanMessage path =
  unlines
    [ "plan.json not found: " <> path
    , ""
    , "Expected input is Cabal's build plan file."
    , "Usually you need to run this in the target project first:"
    , "  cabal build all"
    , ""
    , "Then try again with:"
    , "  cabal-plan-submit inspect-plan dist-newstyle/cache/plan.json"
    ]

inspectDeprecated :: InspectDeprecatedOptions -> IO ()
inspectDeprecated opts = do
  graph <- readGraphOrDie (inspectDeprecatedPlanPath opts)
  eIndex <- readDeprecationIndex (inspectDeprecatedMetadataPath opts)
  case eIndex of
    Left err -> die ("failed to parse deprecated metadata: " <> err)
    Right index -> do
      let deprecated =
            filterDeprecatedPackagesIgnoring
              (inspectDeprecatedIgnoredPackages opts)
              ( findDeprecatedPackagesFrom
                  (inspectDeprecatedLocalFilter opts)
                  index
                  graph
              )
      putStr (renderDeprecatedPackages deprecated)
      when (shouldFailOnDeprecated (inspectDeprecatedFailOn opts) deprecated) $ do
        hPutStrLn stderr (failOnMessage (inspectDeprecatedFailOn opts))
        exitFailure

parseFailOnDeprecated :: String -> Maybe FailOnDeprecated
parseFailOnDeprecated = \case
  "none"   -> Just FailOnNone
  "direct" -> Just FailOnDirect
  "any"    -> Just FailOnAny
  _        -> Nothing

failOnMessage :: FailOnDeprecated -> String
failOnMessage = \case
  FailOnNone   -> ""
  FailOnDirect -> "deprecated direct dependencies found"
  FailOnAny    -> "deprecated dependencies found"

whyPackage :: LocalUnitFilter -> FilePath -> String -> IO ()
whyPackage localFilter path packageName = do
  graph <- readGraphOrDie path
  putStr $
    renderWhyFrom
      localFilter
      (PackageName (Text.pack packageName))
      graph

whyPackageTree :: LocalUnitFilter -> FilePath -> String -> IO ()
whyPackageTree localFilter path packageName = do
  graph <- readGraphOrDie path
  putStr $
    renderWhyTrieFrom
      localFilter
      (PackageName (Text.pack packageName))
      graph

inspectLocalPackages :: FilePath -> IO ()
inspectLocalPackages path = do
  graph <- readGraphOrDie path
  putStr $
    renderLocals
      (inspectLocals graph)

enrichSarif :: LocalUnitFilter -> FilePath -> FilePath -> IO ()
enrichSarif localFilter planPath sarifPath = do
  graph <- readGraphOrDie planPath
  let repoRoot = guessRepoRoot graph
  lineIndex <- buildCabalLineIndex repoRoot graph
  sarifBytes <- readSarifFileOrDie sarifPath

  case Aeson.eitherDecodeStrict' sarifBytes of
    Left err ->
      die ("failed to parse SARIF JSON: " <> err)

    Right sarif ->
      case sarif of
        Aeson.Object o
          | Just (Aeson.Array _) <- KeyMap.lookup "runs" o ->
              LBS8.putStrLn $
                Aeson.encode $
                  enrichSarifValue
                    lineIndex
                    localFilter
                    graph
                    sarif
        _ ->
           die "invalid SARIF JSON: expected top-level object with a 'runs' array"

readSarifFileOrDie :: FilePath -> IO BS.ByteString
readSarifFileOrDie sarifPath = do
  result <- try (BS.readFile sarifPath) :: IO (Either IOException BS.ByteString)
  case result of
    Left err ->
      die $
        unlines
          [ "failed to read SARIF file: " <> sarifPath
          , show err
          , ""
          , "Expected cabal-audit SARIF JSON. Example:"
          , "  cabal-audit --sarif > cabal-audit.sarif"
          ]

    Right bytes ->
      pure bytes

-- line-index helpers

data DeprecatedSarifOptions = DeprecatedSarifOptions
  { deprecatedSarifLocalFilter     :: LocalUnitFilter
  , deprecatedSarifIgnoredPackages :: Set PackageName
  , deprecatedSarifPlanPath        :: FilePath
  , deprecatedSarifMetadataPath    :: FilePath
  }
  deriving stock (Eq, Show)

defaultDeprecatedSarifOptions :: DeprecatedSarifOptions
defaultDeprecatedSarifOptions =
  DeprecatedSarifOptions
    { deprecatedSarifLocalFilter = AllLocalUnits
    , deprecatedSarifIgnoredPackages = Set.empty
    , deprecatedSarifPlanPath = ""
    , deprecatedSarifMetadataPath = ""
    }

parseDeprecatedSarifOptions :: [String] -> Either String DeprecatedSarifOptions
parseDeprecatedSarifOptions =
  go defaultDeprecatedSarifOptions
 where
  go opts =
    \case
      "--production-only" : rest ->
        go opts { deprecatedSarifLocalFilter = ProductionLocalUnits } rest

      "--ignore-package" : pkgName : rest ->
        go
          opts
            { deprecatedSarifIgnoredPackages =
                Set.insert
                  (PackageName (Text.pack pkgName))
                  (deprecatedSarifIgnoredPackages opts)
            }
          rest

      [planPath, metadataPath] ->
        Right
          opts
            { deprecatedSarifPlanPath = planPath
            , deprecatedSarifMetadataPath = metadataPath
            }

      _ ->
        Left usage

deprecatedSarif :: LocalUnitFilter -> Set PackageName -> FilePath -> FilePath -> IO ()
deprecatedSarif localFilter ignoredPackages planPath deprecatedPath = do
  graph <- readGraphOrDie planPath
  eIndex <- readDeprecationIndex deprecatedPath
  case eIndex of
    Left err ->
      die ("failed to parse deprecated metadata: " <> err)

    Right index -> do
      let repoRoot =
            guessRepoRoot graph

      lineIndex <- buildCabalLineIndex repoRoot graph

      let deprecated =
            filterDeprecatedPackagesIgnoring
              ignoredPackages
              (findDeprecatedPackagesFrom localFilter index graph)

      LBS8.putStrLn $
        Aeson.encode $
          deprecatedPackagesSarif
            lineIndex
            repoRoot
            deprecated

usage :: String
usage =
  unlines
    [ "Usage:"
    , "  cabal-plan-submit --help"
    , "  cabal-plan-submit --version"
    , "  cabal-plan-submit inspect-plan [PATH_TO_PLAN_JSON]"
    , "  cabal-plan-submit inspect-graph [PATH_TO_PLAN_JSON]"
    , "  cabal-plan-submit inspect-locals [PATH_TO_PLAN_JSON]"
    , "  cabal-plan-submit render-snapshot PATH_TO_PLAN_JSON SHA REF"
    , "  cabal-plan-submit validate-snapshot PATH_TO_SNAPSHOT_JSON"
    , "  cabal-plan-submit [inspect-]deprecated [--production-only] [--fail-on none|direct|any] [--ignore-package PACKAGE]... PATH_TO_PLAN_JSON PATH_TO_DEPRECATED_YAML"
    , "  cabal-plan-submit why [--production-only] [PATH_TO_PLAN_JSON] PACKAGE_NAME"
    , "  cabal-plan-submit why-tree [--production-only] [PATH_TO_PLAN_JSON] PACKAGE_NAME"
    , "  cabal-plan-submit enrich-sarif [--production-only] PATH_TO_PLAN_JSON PATH_TO_SARIF_JSON"
    , "  cabal-plan-submit deprecated-sarif [--production-only] [--ignore-package PACKAGE]... PATH_TO_PLAN_JSON PATH_TO_DEPRECATED_YAML"
    ]

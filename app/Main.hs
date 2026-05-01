{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.List (isSuffixOf, sort)
import Data.Text qualified as Text
import Data.Time.Clock (getCurrentTime)
import Data.Version (showVersion)
import Hgs.Deprecated
  ( findDeprecatedPackages
  , readDeprecationIndex
  , renderDeprecatedPackages
  )
import Hgs.Domain (RawPlan)
import Hgs.Extract (extractPlanGraph, summarisePlanGraph)
import Hgs.Input.PlanJson (readRawPlan, summariseRawPlan)
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
import Hgs.Domain (PackageName(..))
import Hgs.Why (renderWhy)
import Paths_cabal_plan_submit qualified as Paths
import System.Directory (doesFileExist, listDirectory)
import System.Environment (getArgs)
import System.Exit (die, exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] ->
      putStr usage
    ["-h"] ->
      putStr usage
    ["--version"] ->
      putStrLn ("cabal-plan-submit " <> showVersion Paths.version)
    ["inspect-plan", path] ->
      inspectPlan path
    ["inspect-graph", path] ->
      inspectGraph path
    ["render-snapshot", path, sha, ref] ->
      renderSnapshot path sha ref
    ["validate-snapshot", path] ->
      validateSnapshot path
    ["inspect-deprecated", planPath, deprecatedPath] ->
      inspectDeprecated planPath deprecatedPath
    ["why", path, packageName] ->
      whyPackage path packageName
    _ ->
      die usage

inspectPlan :: FilePath -> IO ()
inspectPlan path = do
  plan <- readPlanOrDie path
  putStrLn (summariseRawPlan plan)

inspectGraph :: FilePath -> IO ()
inspectGraph path = do
  plan <- readPlanOrDie path
  putStrLn (summarisePlanGraph (extractPlanGraph plan))

renderSnapshot :: FilePath -> String -> String -> IO ()
renderSnapshot path sha ref = do
  plan <- readPlanOrDie path
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
        snapshotFromPlanGraph input (extractPlanGraph plan)
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

inspectDeprecated :: FilePath -> FilePath -> IO ()
inspectDeprecated planPath deprecatedPath = do
  plan <- readPlanOrDie planPath
  eIndex <- readDeprecationIndex deprecatedPath
  case eIndex of
    Left err ->
      die ("failed to parse deprecated metadata: " <> err)
    Right index ->
      putStr $
        renderDeprecatedPackages
          (findDeprecatedPackages index (extractPlanGraph plan))

whyPackage :: FilePath -> String -> IO ()
whyPackage path packageName = do
  plan <- readPlanOrDie path
  putStr $
    renderWhy
      (PackageName (Text.pack packageName))
      (extractPlanGraph plan)

usage :: String
usage =
  unlines
    [ "Usage:"
    , "  cabal-plan-submit --help"
    , "  cabal-plan-submit --version"
    , "  cabal-plan-submit inspect-plan PATH_TO_PLAN_JSON"
    , "  cabal-plan-submit inspect-graph PATH_TO_PLAN_JSON"
    , "  cabal-plan-submit render-snapshot PATH_TO_PLAN_JSON SHA REF"
    , "  cabal-plan-submit validate-snapshot PATH_TO_SNAPSHOT_JSON"
    , "  cabal-plan-submit inspect-deprecated PATH_TO_PLAN_JSON PATH_TO_DEPRECATED_YAML"
    , "  cabal-plan-submit why PATH_TO_PLAN_JSON PACKAGE_NAME"
    ]

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (when)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.List (isSuffixOf, sort)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time.Clock (getCurrentTime)
import Data.Version (showVersion)
import Hgs.Deprecated
  ( FailOnDeprecated(..)
  , filterDeprecatedPackagesIgnoring
  , findDeprecatedPackagesFrom
  , readDeprecationIndex
  , renderDeprecatedPackages
  , shouldFailOnDeprecated
  )
import Hgs.Domain (RawPlan)
import Hgs.Extract (extractPlanGraph, summarisePlanGraph)
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
import Hgs.Domain (PackageName(..))
import Hgs.Why (renderWhyFrom)
import Hgs.LocalUnitFilter
  ( LocalUnitFilter(..)
  )
import Paths_cabal_plan_submit qualified as Paths
import System.Directory (doesFileExist, listDirectory)
import System.Environment (getArgs)
import System.Exit (die, exitFailure)
import System.IO (hPutStrLn, stderr)

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
    "inspect-deprecated" : rest ->
      case parseInspectDeprecatedOptions rest of
        Left err   -> die err
        Right opts -> inspectDeprecated opts
    ["why", "--production-only", path, packageName] ->
      whyPackage ProductionLocalUnits path packageName
    ["why", path, packageName] ->
      whyPackage AllLocalUnits path packageName
    ["inspect-locals", path] ->
      inspectLocalPackages path
    _ ->
      die usage


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

inspectDeprecated :: InspectDeprecatedOptions -> IO ()
inspectDeprecated opts = do
  plan <- readPlanOrDie (inspectDeprecatedPlanPath opts)
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
                  (extractPlanGraph plan)
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
  plan <- readPlanOrDie path
  putStr $
    renderWhyFrom
      localFilter
      (PackageName (Text.pack packageName))
      (extractPlanGraph plan)

inspectLocalPackages :: FilePath -> IO ()
inspectLocalPackages path = do
  plan <- readPlanOrDie path
  putStr $
    renderLocals
      (inspectLocals (extractPlanGraph plan))

usage :: String
usage =
  unlines
    [ "Usage:"
    , "  cabal-plan-submit --help"
    , "  cabal-plan-submit --version"
    , "  cabal-plan-submit inspect-plan PATH_TO_PLAN_JSON"
    , "  cabal-plan-submit inspect-graph PATH_TO_PLAN_JSON"
    , "  cabal-plan-submit inspect-locals PATH_TO_PLAN_JSON"
    , "  cabal-plan-submit render-snapshot PATH_TO_PLAN_JSON SHA REF"
    , "  cabal-plan-submit validate-snapshot PATH_TO_SNAPSHOT_JSON"
    , "  cabal-plan-submit inspect-deprecated [--production-only] [--fail-on none|direct|any] [--ignore-package PACKAGE]... PATH_TO_PLAN_JSON PATH_TO_DEPRECATED_YAML"
    , "  cabal-plan-submit why PATH_TO_PLAN_JSON PACKAGE_NAME"
    , "  cabal-plan-submit why --production-only PATH_TO_PLAN_JSON PACKAGE_NAME"
    ]

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
import Data.Char (isAlphaNum)
import qualified Data.Map as Map
import Data.Maybe (maybeToList)
import Data.List (isSuffixOf, sort)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time.Clock (getCurrentTime)
import Data.Version (showVersion)
import Paths_cabal_plan_submit qualified as Paths
import System.Directory (doesFileExist, listDirectory)
import System.Environment (getArgs)
import System.FilePath
  ( (</>)
  , dropTrailingPathSeparator
  , joinPath
  , makeRelative
  , normalise
  , takeExtension
  , splitDirectories
  )
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
  ( Package(..)
  , PackageName(..)
  , PackageSource(..)
  , PlanGraph(..)
  , RawPlan
  )
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
import Hgs.Why (renderWhyFrom)
import Hgs.LocalUnitFilter
  ( LocalUnitFilter(..)
  )
import Hgs.Sarif.Deprecated
  ( deprecatedPackagesSarif
  )
import Hgs.Sarif.Enrich
  ( CabalLineIndex
  , enrichSarifValue
  )

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
    ["enrich-sarif", planPath, sarifPath] ->
      enrichSarif AllLocalUnits planPath sarifPath
    ["enrich-sarif", "--production-only", planPath, sarifPath] ->
      enrichSarif ProductionLocalUnits planPath sarifPath
    "deprecated-sarif" : rest ->
      case parseDeprecatedSarifOptions rest of
        Left err ->
          die err
        Right opts ->
          deprecatedSarif
            (deprecatedSarifLocalFilter opts)
            (deprecatedSarifIgnoredPackages opts)
            (deprecatedSarifPlanPath opts)
            (deprecatedSarifMetadataPath opts)
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

enrichSarif :: LocalUnitFilter -> FilePath -> FilePath -> IO ()
enrichSarif localFilter planPath sarifPath = do
  plan <- readPlanOrDie planPath
  let graph = extractPlanGraph plan
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

buildCabalLineIndex :: FilePath -> PlanGraph -> IO CabalLineIndex
buildCabalLineIndex repoRoot graph =
  fmap Map.unions $
    traverse (cabalLineIndexForLocalPackage repoRoot) localPackages
 where
  localPackages =
    [ pkg
    | pkg <- Map.elems (planGraphPackages graph)
    , packageSource pkg == PackageLocal
    ]

cabalLineIndexForLocalPackage :: FilePath -> Package -> IO CabalLineIndex
cabalLineIndexForLocalPackage repoRoot pkg =
  case packageSourcePath pkg of
    Nothing ->
      pure Map.empty

    Just sourcePath -> do
      let cabalFile =
            localPackageCabalFile pkg sourcePath
      exists <- doesFileExist cabalFile
      if exists
        then do
          contents <- readFile cabalFile
          pure (indexCabalFile repoRoot cabalFile contents)
        else
          pure Map.empty

indexCabalFile :: FilePath -> FilePath -> String -> CabalLineIndex
indexCabalFile repoRoot cabalFile contents =
  Map.fromListWith min
    [ ( (repoRelativeCabalFile repoRoot cabalFile, PackageName (Text.pack token))
      , lineNo
      )
    | (lineNo, line) <- zip [1 :: Int ..] (lines contents)
    , token <- packageTokens line
    ]

packageTokens :: String -> [String]
packageTokens =
  filter plausiblePackageName
    . wordsBy (not . isPackageNameChar)
    . stripLineComment

stripLineComment :: String -> String
stripLineComment =
  takeUntilDashDash
 where
  takeUntilDashDash [] =
    []

  takeUntilDashDash [x] =
    [x]

  takeUntilDashDash ('-' : '-' : _) =
    []

  takeUntilDashDash (x : xs) =
    x : takeUntilDashDash xs

isPackageNameChar :: Char -> Bool
isPackageNameChar c =
  isAlphaNum c || c == '-' || c == '_'

plausiblePackageName :: String -> Bool
plausiblePackageName token =
  '-' `elem` token || all isAlphaNum token

wordsBy :: (Char -> Bool) -> String -> [String]
wordsBy p =
  go
 where
  go [] =
    []

  go xs =
    case dropWhile p xs of
      [] ->
        []

      ys ->
        let (word, rest) = break p ys
         in word : go rest

-- cabal file helpers

localPackageCabalFile :: Package -> FilePath -> FilePath
localPackageCabalFile pkg path
  | takeExtension path == ".cabal" =
      normalise path

  | otherwise =
      normalise path </> Text.unpack (unPackageName (packageName pkg)) <> ".cabal"

repoRelativeCabalFile :: FilePath -> FilePath -> FilePath
repoRelativeCabalFile repoRoot path =
  normalise $
    makeRelative
      (normalise repoRoot)
      (normalise path)

guessRepoRoot :: PlanGraph -> FilePath
guessRepoRoot graph =
  case localSourcePaths of
    [] ->
      "."

    [path] ->
      dropTrailingPathSeparator (normalise path)

    path : rest ->
      foldl commonPrefixPath path rest
 where
  localSourcePaths =
    [ dropTrailingPathSeparator (normalise path)
    | pkg <- Map.elems (planGraphPackages graph)
    , packageSource pkg == PackageLocal
    , path <- maybeToList (packageSourcePath pkg)
    ]

commonPrefixPath :: FilePath -> FilePath -> FilePath
commonPrefixPath a b =
  joinPathParts $
    map fst $
      takeWhile (uncurry (==)) $
        zip (splitPathParts a) (splitPathParts b)

splitPathParts :: FilePath -> [FilePath]
splitPathParts =
  splitDirectories . normalise

joinPathParts :: [FilePath] -> FilePath
joinPathParts =
  \case
    [] -> "."
    xs -> joinPath xs

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
  plan <- readPlanOrDie planPath
  eIndex <- readDeprecationIndex deprecatedPath
  case eIndex of
    Left err ->
      die ("failed to parse deprecated metadata: " <> err)

    Right index -> do
      let graph =
            extractPlanGraph plan

          repoRoot =
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
    , "  cabal-plan-submit inspect-plan PATH_TO_PLAN_JSON"
    , "  cabal-plan-submit inspect-graph PATH_TO_PLAN_JSON"
    , "  cabal-plan-submit inspect-locals PATH_TO_PLAN_JSON"
    , "  cabal-plan-submit render-snapshot PATH_TO_PLAN_JSON SHA REF"
    , "  cabal-plan-submit validate-snapshot PATH_TO_SNAPSHOT_JSON"
    , "  cabal-plan-submit inspect-deprecated [--production-only] [--fail-on none|direct|any] [--ignore-package PACKAGE]... PATH_TO_PLAN_JSON PATH_TO_DEPRECATED_YAML"
    , "  cabal-plan-submit why PATH_TO_PLAN_JSON PACKAGE_NAME"
    , "  cabal-plan-submit why --production-only PATH_TO_PLAN_JSON PACKAGE_NAME"
    , "  cabal-plan-submit enrich-sarif PATH_TO_PLAN_JSON PATH_TO_SARIF_JSON"
    , "  cabal-plan-submit enrich-sarif --production-only PATH_TO_PLAN_JSON PATH_TO_SARIF_JSON"
    , "  cabal-plan-submit deprecated-sarif [--production-only] [--ignore-package PACKAGE]... PATH_TO_PLAN_JSON PATH_TO_DEPRECATED_YAML"
    ]

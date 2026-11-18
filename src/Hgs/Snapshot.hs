{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Hgs.Snapshot
  ( Purl(..)
  , SnapshotInput(..)
  , snapshotFromPlanGraph
  , encodeSnapshot
  ) where

import Data.Aeson
  ( ToJSON(toJSON)
  , encode
  , object
  , (.=)
  )
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Containers.ListUtils (nubOrd)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Hgs.Domain
  ( ComponentName(..)
  , DependencyKind(..)
  , DependencyOrigin(..)
  , Package(..)
  , PackageName(..)
  , PackageSource(..)
  , PlanGraph(..)
  , UnitId(..)
  , Version(..)
  , packageDepends
  )
import Hgs.Reachability (reachableUnits)
import Hgs.DependencyUsage (Usage(..), dependencyUsages, packageUsagesFrom, snapshotScope)

newtype Purl = Purl { unPurl :: Text }
  deriving stock (Eq, Ord, Show)

data SnapshotInput = SnapshotInput
  { snapshotSha             :: Text
  , snapshotRef             :: Text
  , snapshotScannedAt       :: UTCTime
  , snapshotJobId           :: Text
  , snapshotCorrelator      :: Text
  , snapshotManifestKey     :: Text
  , snapshotManifestName    :: Text
  , snapshotManifestPath    :: Maybe FilePath
  , snapshotDetectorName    :: Text
  , snapshotDetectorVersion :: Text
  , snapshotDetectorUrl     :: Text
  }
  deriving stock (Eq, Show)

data Snapshot = Snapshot
  { outVersion   :: Int
  , outSha       :: Text
  , outRef       :: Text
  , outJob       :: Job
  , outDetector  :: Detector
  , outScanned   :: UTCTime
  , outManifests :: Map Text Manifest
  }
  deriving stock (Eq, Show)

data Job = Job
  { jobId         :: Text
  , jobCorrelator :: Text
  }
  deriving stock (Eq, Show)

data Detector = Detector
  { detectorName    :: Text
  , detectorVersion :: Text
  , detectorUrl     :: Text
  }
  deriving stock (Eq, Show)

data Manifest = Manifest
  { manifestName     :: Text
  , manifestFilePath :: Maybe FilePath
  , manifestResolved :: Map Text Resolved
  }
  deriving stock (Eq, Show)

data Resolved = Resolved
  { resolvedPackageUrl   :: Purl
  , resolvedRelationship :: Text
  , resolvedDependencies :: [Purl]
  , resolvedScope        :: Maybe Text
  , resolvedUnmergedUnits :: Maybe Text
  }
  deriving stock (Eq, Show)

snapshotFromPlanGraph :: SnapshotInput -> PlanGraph -> Snapshot
snapshotFromPlanGraph input graph =
  Snapshot
    { outVersion = 0
    , outSha = snapshotSha input
    , outRef = snapshotRef input
    , outJob =
        Job
          { jobId = snapshotJobId input
          , jobCorrelator = snapshotCorrelator input
          }
    , outDetector =
        Detector
          { detectorName = snapshotDetectorName input
          , detectorVersion = snapshotDetectorVersion input
          , detectorUrl = snapshotDetectorUrl input
          }
    , outScanned = snapshotScannedAt input
    , outManifests =
        Map.singleton
          (snapshotManifestKey input)
          (manifestFromGraph input graph)
    }

encodeSnapshot :: Snapshot -> ByteString
encodeSnapshot = encode

manifestFromGraph :: SnapshotInput -> PlanGraph -> Manifest
manifestFromGraph input graph =
  Manifest
    { manifestName = snapshotManifestName input
    , manifestFilePath = snapshotManifestPath input >>= nonEmptyPath
    , manifestResolved =
        Map.mapMaybe mergeUnits packagesByPurl
    }
 where
  pkgs :: [Package]
  pkgs =
    Map.elems (planGraphPackages graph)

  externalPackages :: [Package]
  externalPackages =
    filter ((== PackageExternal) . packageSource) pkgs

  packagesByPurl :: Map Text [Package]
  packagesByPurl = Map.fromListWith (<>) [(unPurl (packagePurl pkg), [pkg]) | pkg <- externalPackages]

  -- A PURL cannot distinguish units. Prefer production/unknown units when
  -- a production path exists; preserve the other units in snapshot metadata.
  selectUnits :: [Package] -> [Package]
  selectUnits units
    | any (hasUsage Production) relevant = filter productionOrUnknown relevant
    | otherwise = relevant
   where
    relevant = case filter ((`Set.member` reachable) . packageUnitId) units of
      [] -> units
      selected -> selected
    productionOrUnknown pkg = hasUsage Production pkg || hasUsage Unknown pkg

  hasUsage usage pkg = usage `Set.member` Map.findWithDefault Set.empty (packageUnitId pkg) unitUsages

  reachable = reachableUnits graph
  unitUsages = dependencyUsages graph

  mergeUnits :: [Package] -> Maybe Resolved
  mergeUnits units = case selectUnits units of
    [] -> Nothing
    unit : selected -> Just $
      (foldl mergeResolved (resolve unit) (map resolve selected))
          { resolvedScope = snapshotScope (Map.findWithDefault Set.empty (packageName unit, packageVersion unit) usages)
          , resolvedRelationship = if any packageIsDirect units then "direct" else "indirect"
          , resolvedUnmergedUnits = encodeUnmerged (catMaybes (map (unmergedEvidence (unit : selected)) units))
          }
   where
    resolve = resolvedFromPackage purlByUnitId externalUnitIds . selectedOrigins

  selectedOrigins :: Package -> Package
  selectedOrigins pkg
    | hasUsage Production pkg = pkg {packageDependencies = Map.mapMaybe keepOrigins (packageDependencies pkg)}
    | otherwise = pkg
   where
    keepOrigins origins =
      let kept = Set.filter (not . developmentOrigin) origins
       in if Set.null kept then Nothing else Just kept

  developmentOrigin :: DependencyOrigin -> Bool
  developmentOrigin origin = case dependencyComponent origin of
    Just (ComponentName name) -> any (`Text.isPrefixOf` name) ["test:", "bench:"]
    Nothing -> False

  unmergedEvidence :: [Package] -> Package -> Maybe Package
  unmergedEvidence selected pkg
    | pkg `notElem` selected = Just pkg
    | Map.null omitted = Nothing
    | otherwise = Just pkg {packageDependencies = omitted}
   where
    omitted = Map.differenceWith difference (packageDependencies pkg) (packageDependencies (selectedOrigins pkg))
    difference original kept =
      let remaining = original Set.\\ kept
       in if Set.null remaining then Nothing else Just remaining

  usages = packageUsagesFrom graph unitUsages

  externalUnitIds :: Set.Set UnitId
  externalUnitIds =
    Set.fromList (map packageUnitId externalPackages)

  purlByUnitId :: Map UnitId Purl
  purlByUnitId =
    Map.fromList
      [ (packageUnitId pkg, packagePurl pkg)
      | pkg <- externalPackages
      ]

encodeUnmerged :: [Package] -> Maybe Text
encodeUnmerged [] = Nothing
encodeUnmerged units = Just $ Text.decodeUtf8 $ LBS.toStrict $ encode (map unitEvidence units)

unitEvidence :: Package -> Aeson.Value
unitEvidence pkg = object
  [ "unit" .= unUnitId (packageUnitId pkg)
  , "component" .= fmap unComponentName (packageComponentName pkg)
  , "dependencies" .=
      [ object ["unit" .= unUnitId target, "origins" .= map originEvidence (Set.toAscList origins)]
      | (target, origins) <- Map.toAscList (packageDependencies pkg)
      ]
  ]

originEvidence :: DependencyOrigin -> Aeson.Value
originEvidence origin = object
  [ "component" .= fmap unComponentName (dependencyComponent origin)
  , "field" .= case dependencyKind origin of
      ComponentDependency -> ("depends" :: Text)
      ExecutableDependency -> "exe-depends"
  ]

-- A PURL identifies a package version, not an individual Cabal unit.
-- Combine dependencies and directness rather than discarding a component.
mergeResolved :: Resolved -> Resolved -> Resolved
mergeResolved a b =
  a
    { resolvedRelationship =
        if resolvedRelationship a == "direct" || resolvedRelationship b == "direct"
          then "direct"
          else "indirect"
    , resolvedDependencies =
        nubOrd (resolvedDependencies a <> resolvedDependencies b)
    }

nonEmptyPath :: FilePath -> Maybe FilePath
nonEmptyPath path
  | null path = Nothing
  | otherwise = Just path

resolvedFromPackage :: Map UnitId Purl -> Set.Set UnitId -> Package -> Resolved
resolvedFromPackage purlByUnitId externalUnitIds pkg =
  Resolved
    { resolvedPackageUrl = pkgPurl
    , resolvedScope = Nothing
    , resolvedUnmergedUnits = Nothing
    , resolvedRelationship =
        if packageIsDirect pkg then "direct" else "indirect"
    , resolvedDependencies =
        nubOrd
          [ depPurl
          | depUnitId <- Set.toAscList (Set.filter (`Set.member` externalUnitIds) (packageDepends pkg))
          , depPurl <- maybeToList (Map.lookup depUnitId purlByUnitId)
          , depPurl /= pkgPurl
          ]
    }
 where
  pkgPurl = packagePurl pkg

packagePurl :: Package -> Purl
packagePurl pkg =
  Purl $
    "pkg:hackage/"
      <> purlName (packageName pkg)
      <> "@"
      <> unVersion (packageVersion pkg)

purlName :: PackageName -> Text
purlName =
  percentEncode . unPackageName

percentEncode :: Text -> Text
percentEncode =
  Text.concatMap encodeChar
 where
  encodeChar c
    | isUnreserved c = Text.singleton c
    | otherwise = pct c

  isUnreserved c =
    isAsciiUpper c
      || isAsciiLower c
      || isDigit c
      || c `elem` ("-._~" :: String)

  pct c =
    let n = fromEnum c
        hex = "0123456789ABCDEF"
        hi = Text.singleton (hex !! (n `div` 16))
        lo = Text.singleton (hex !! (n `mod` 16))
     in "%" <> hi <> lo

maybeToList :: Maybe a -> [a]
maybeToList =
  maybe [] pure

instance ToJSON Snapshot where
  toJSON snapshot =
    object
      [ "version" .= outVersion snapshot
      , "sha" .= outSha snapshot
      , "ref" .= outRef snapshot
      , "job" .= outJob snapshot
      , "detector" .= outDetector snapshot
      , "scanned" .= renderUtc (outScanned snapshot)
      , "manifests" .= outManifests snapshot
      ]

instance ToJSON Job where
  toJSON job =
    object
      [ "id" .= jobId job
      , "correlator" .= jobCorrelator job
      ]

instance ToJSON Detector where
  toJSON detector =
    object
      [ "name" .= detectorName detector
      , "version" .= detectorVersion detector
      , "url" .= detectorUrl detector
      ]

instance ToJSON Manifest where
  toJSON manifest =
    object $
      [ "name" .= manifestName manifest
      , "resolved" .= manifestResolved manifest
      ]
        <> catMaybes
          [ fmap (\path -> "file" .= object ["source_location" .= path]) (manifestFilePath manifest)
          ]

instance ToJSON Resolved where
  toJSON resolved =
    object $
      [ "package_url" .= resolvedPackageUrl resolved
      , "relationship" .= resolvedRelationship resolved
      , "dependencies" .= resolvedDependencies resolved
      ]
      <> maybe [] (\scope -> ["scope" .= scope]) (resolvedScope resolved)
      <> maybe [] (\units -> ["metadata" .= object ["cabal-plan-submit.unmerged-units" .= units]]) (resolvedUnmergedUnits resolved)

instance ToJSON Purl where
  toJSON = toJSON . unPurl

renderUtc :: UTCTime -> String
renderUtc = iso8601Show

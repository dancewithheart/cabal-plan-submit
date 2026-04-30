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
import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Hgs.Domain
  ( Package(..)
  , PackageName(..)
  , PackageSource(..)
  , PlanGraph(..)
  , UnitId
  , Version(..)
  )

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
  , resolvedScope        :: Text
  , resolvedDependencies :: [Purl]
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
    , manifestFilePath = snapshotManifestPath input
    , manifestResolved =
        Map.fromList
          [ (unPurl (packagePurl pkg), resolvedFromPackage purlByUnitId externalUnitIds pkg)
          | pkg <- externalPackages
          ]
    }
 where
  pkgs :: [Package]
  pkgs =
    Map.elems (planGraphPackages graph)

  externalPackages :: [Package]
  externalPackages =
    filter ((== PackageExternal) . packageSource) pkgs

  externalUnitIds :: Set.Set UnitId
  externalUnitIds =
    Set.fromList (map packageUnitId externalPackages)

  purlByUnitId :: Map UnitId Purl
  purlByUnitId =
    Map.fromList
      [ (packageUnitId pkg, packagePurl pkg)
      | pkg <- externalPackages
      ]

resolvedFromPackage :: Map UnitId Purl -> Set.Set UnitId -> Package -> Resolved
resolvedFromPackage purlByUnitId externalUnitIds pkg =
  Resolved
    { resolvedPackageUrl = pkgPurl
    , resolvedRelationship =
        if packageIsDirect pkg then "direct" else "indirect"
    , resolvedScope = "runtime"
    , resolvedDependencies =
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
    ('A' <= c && c <= 'Z')
      || ('a' <= c && c <= 'z')
      || ('0' <= c && c <= '9')
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
    object
      [ "package_url" .= resolvedPackageUrl resolved
      , "relationship" .= resolvedRelationship resolved
      , "scope" .= resolvedScope resolved
      , "dependencies" .= resolvedDependencies resolved
      ]

instance ToJSON Purl where
  toJSON = toJSON . unPurl

renderUtc :: UTCTime -> String
renderUtc = iso8601Show

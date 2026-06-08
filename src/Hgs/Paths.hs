{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Hgs.Paths
  ( PackagePath(..)
  , PackageKey
  , packageKey
  , packagePathKey
  , normalizePackagePath
  , collapseAdjacentSamePackages
  , renderPackagePath
  ) where

import Data.List (intercalate)
import Hgs.Domain
  ( Package(..)
  , PackageName(..)
  , Version(..)
  )
import Hgs.PrettyPrint (renderPackage)

newtype PackagePath = PackagePath { unPackagePath :: [Package] }
  deriving stock (Eq, Show)

type PackageKey =
  (PackageName, Version)

packageKey :: Package -> PackageKey
packageKey pkg =
  (packageName pkg, packageVersion pkg)

packagePathKey :: PackagePath -> [PackageKey]
packagePathKey =
  map packageKey . unPackagePath

normalizePackagePath :: PackagePath -> PackagePath
normalizePackagePath =
  PackagePath . collapseAdjacentSamePackages . unPackagePath

collapseAdjacentSamePackages :: [Package] -> [Package]
collapseAdjacentSamePackages =
  \case
    [] ->
      []

    x : xs ->
      x : go x xs
 where
  go _ [] =
    []

  go previous (x : xs)
    | packageKey previous == packageKey x =
        go previous xs
    | otherwise =
        x : go x xs

renderPackagePath :: PackagePath -> String
renderPackagePath (PackagePath packages) =
  intercalate " -> " (map renderPackage packages)

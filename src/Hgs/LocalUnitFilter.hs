{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Hgs.LocalUnitFilter
  ( LocalUnitFilter(..)
  , localUnitAllowed
  , dependencyUnits
  ) where

import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Hgs.Domain
  ( ComponentName(..)
  , DependencyOrigin(..)
  , Package(..)
  , PackageSource(..)
  , UnitId(..)
  , packageDepends
  )

data LocalUnitFilter
  = AllLocalUnits
  | ProductionLocalUnits
  deriving stock (Eq, Show)

localUnitAllowed :: LocalUnitFilter -> Package -> Bool
localUnitAllowed filterKind pkg =
  case filterKind of
    AllLocalUnits ->
      True

    ProductionLocalUnits ->
      maybe True productionComponent (packageComponentName pkg)

-- Unknown component provenance is retained: omitting it could hide a
-- production dependency. Build tools in production components remain edges.
dependencyUnits :: LocalUnitFilter -> Package -> Set UnitId
dependencyUnits ProductionLocalUnits pkg
  | packageSource pkg == PackageLocal =
      Map.keysSet $ Map.filter (any (maybe True productionComponent . dependencyComponent) . Set.toList)
        (packageDependencies pkg)
dependencyUnits _ pkg = packageDepends pkg

productionComponent :: ComponentName -> Bool
productionComponent (ComponentName name) =
  not ("test:" `Text.isPrefixOf` name || "bench:" `Text.isPrefixOf` name)

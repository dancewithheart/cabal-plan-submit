{-# LANGUAGE OverloadedStrings #-}

module Hgs.DependencyUsage
  ( Usage(..)
  , dependencyUsages
  , packageUsages
  , snapshotScope
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Hgs.Domain
  ( ComponentName(..), DependencyKind(..), DependencyOrigin(..), Package(..)
  , PackageName, PlanGraph(..), UnitId, Version
  )
import Hgs.Reachability (closure)

-- These describe paths in the plan, not whether vulnerable code executes.
data Usage = Production | Test | Benchmark | Build | Unknown
  deriving (Eq, Ord, Show)

dependencyUsages :: PlanGraph -> Map UnitId (Set Usage)
dependencyUsages graph = Map.fromListWith Set.union
  [ (unit, Set.singleton usage) | (unit, usage) <- Set.toList reached ]
 where
  packages = planGraphPackages graph
  seeds = Set.fromList
    [ (packageUnitId pkg, rootUsage pkg)
    | pkg <- Map.elems packages
    , packageUnitId pkg `Set.member` planGraphLocals graph
    ]
  reached = closure next seeds
  next (unit, usage) = case Map.lookup unit packages of
    Nothing -> Set.empty
    Just pkg -> Set.fromList
      [ (target, follow usage origin)
      | (target, origins) <- Map.toList (packageDependencies pkg)
      , origin <- Set.toList origins
      ]

rootUsage :: Package -> Usage
rootUsage pkg = case packageComponentName pkg of
  Just component -> componentUsage component
  -- A combined local unit has independently identified component edges.
  Nothing | any hasComponent (concatMap Set.toList (Map.elems (packageDependencies pkg))) -> Production
  Nothing -> Unknown
 where
  hasComponent = maybe False (const True) . dependencyComponent

follow :: Usage -> DependencyOrigin -> Usage
follow _ (DependencyOrigin _ ExecutableDependency) = Build
follow parent (DependencyOrigin component ComponentDependency) =
  case componentUsage <$> component of
    Just Build -> Build
    Just Test -> inDevelopment Test parent
    Just Benchmark -> inDevelopment Benchmark parent
    Just Production -> parent
    _ -> case parent of
      Production -> Unknown
      other -> other
 where
  inDevelopment _ Build = Build
  inDevelopment _ Test = Test
  inDevelopment _ Benchmark = Benchmark
  inDevelopment selected _ = selected

componentUsage :: ComponentName -> Usage
componentUsage (ComponentName name)
  | name == "lib" || any (`Text.isPrefixOf` name) ["lib:", "exe:", "flib:"] = Production
  | "test:" `Text.isPrefixOf` name = Test
  | "bench:" `Text.isPrefixOf` name = Benchmark
  | name == "setup" = Build
  | otherwise = Unknown

-- PURLs merge units by package and version. Set union is the join: a
-- production path is never lost just because another unit is test-only.
packageUsages :: PlanGraph -> Map (PackageName, Version) (Set Usage)
packageUsages graph = Map.fromListWith Set.union
  [ ((packageName pkg, packageVersion pkg), uses)
  | (unit, uses) <- Map.toList (dependencyUsages graph)
  , Just pkg <- [Map.lookup unit (planGraphPackages graph)]
  ]

snapshotScope :: Set Usage -> Maybe Text
snapshotScope uses
  | Production `Set.member` uses = Just "runtime"
  | not (Set.null uses) && uses `Set.isSubsetOf` Set.fromList [Test, Benchmark] = Just "development"
  | otherwise = Nothing

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Hgs.Why
  ( PackagePath(..)
  , shortestPathsToPackage
  , renderWhy
  , renderWhyFrom
  , renderPackagePath
  , shortestPathsToPackageFrom
  , renderWhyTrieFrom
  ) where

import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq((:<|)), (|>))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as Text
import Hgs.Domain
  ( Package(..)
  , PackageName(..)
  , PackageSource(..)
  , PlanGraph(..)
  , UnitId
  )
import Hgs.LocalUnitFilter
  ( LocalUnitFilter(..)
  , localUnitAllowed
  )
import Hgs.Paths
  ( PackagePath(..)
  , normalizePackagePath
  , packagePathKey
  , renderPackagePath
  )
import Hgs.PathTrie
  ( pathTrieFromPaths
  , renderPathTrie
  )

shortestPathsToPackage :: PackageName -> PlanGraph -> [PackagePath]
shortestPathsToPackage =
  shortestPathsToPackageFrom AllLocalUnits

shortestPathsToPackageFrom :: LocalUnitFilter -> PackageName -> PlanGraph -> [PackagePath]
shortestPathsToPackageFrom filterKind target graph =
  nubOn packagePathKey $
    map normalizePackagePath $
      mapMaybe pathToLocalRoot roots
 where
  packages = planGraphPackages graph
  roots =
    [ pkg
    | pkg <- Map.elems packages
    , packageSource pkg == PackageLocal
    , localUnitAllowed filterKind pkg
    ]

  pathToLocalRoot =
    bfs packages target

nubOn :: Ord b => (a -> b) -> [a] -> [a]
nubOn f =
  go Set.empty
 where
  go _ [] =
    []
  go seen (x : xs)
    | key `Set.member` seen =
        go seen xs
    | otherwise =
        x : go (Set.insert key seen) xs
   where
    key =
      f x

bfs :: Map.Map UnitId Package -> PackageName -> Package -> Maybe PackagePath
bfs packages target root =
  go Set.empty (Seq.singleton [root])
 where
  go seen queue =
    case queue of
      Seq.Empty ->
        Nothing

      path@(pkg : _) :<| rest
        | packageName pkg == target ->
            Just (PackagePath (reverse path))

        | packageUnitId pkg `Set.member` seen ->
            go seen rest

        | otherwise ->
            let seen' =
                  Set.insert (packageUnitId pkg) seen

                nextPkgs =
                  [ depPkg
                  | depUnitId <- Set.toAscList (packageDepends pkg)
                  , depPkg <- maybeToList (Map.lookup depUnitId packages)
                  ]

                rest' =
                  foldl (\q depPkg -> q |> (depPkg : path)) rest nextPkgs
             in go seen' rest'

      [] :<| rest ->
        go seen rest

renderWhy :: PackageName -> PlanGraph -> String
renderWhy =
  renderWhyFrom AllLocalUnits

renderWhyFrom :: LocalUnitFilter -> PackageName -> PlanGraph -> String
renderWhyFrom filterKind target graph =
  case shortestPathsToPackageFrom filterKind target graph of
    [] -> "no path found to " <> Text.unpack (unPackageName target) <> "\n"
    paths -> unlines $
        [ Text.unpack (unPackageName target) , "paths:" ]
          <> map (("  " <>) . renderPackagePath) paths

renderWhyTrieFrom :: LocalUnitFilter -> PackageName -> PlanGraph -> String
renderWhyTrieFrom filterKind target graph =
  case pathTrieFromPaths (shortestPathsToPackageFrom filterKind target graph) of
    Nothing ->
      "no path found to " <> Text.unpack (unPackageName target) <> "\n"

    Just trie ->
      Text.unpack (unPackageName target)
        <> "\ndependency tree:\n"
        <> renderPathTrie trie

maybeToList :: Maybe a -> [a]
maybeToList =
  maybe [] pure

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Hgs.Why
  ( PackagePath(..)
  , shortestPathsToPackage
  , renderWhy
  , renderPackagePath
  ) where

import Data.List (intercalate, nub)
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
  , Version(..)
  )

newtype PackagePath = PackagePath
  { unPackagePath :: [Package]
  }
  deriving stock (Eq, Show)

shortestPathsToPackage :: PackageName -> PlanGraph -> [PackagePath]
shortestPathsToPackage target graph =
  nub (mapMaybe pathToLocalRoot roots)
 where
  packages =
    planGraphPackages graph

  roots =
    [ pkg
    | pkg <- Map.elems packages
    , packageSource pkg == PackageLocal
    ]

  pathToLocalRoot root =
    bfs packages target root

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
renderWhy target graph =
  case shortestPathsToPackage target graph of
    [] ->
      "no path found to " <> Text.unpack (unPackageName target) <> "\n"

    paths ->
      unlines $
        [ Text.unpack (unPackageName target)
        , "paths:"
        ]
          <> map (("  " <>) . renderPackagePath) paths

renderPackagePath :: PackagePath -> String
renderPackagePath =
  intercalate " -> " . map renderPackage . unPackagePath

renderPackage :: Package -> String
renderPackage pkg =
  Text.unpack (unPackageName (packageName pkg))
    <> "-"
    <> Text.unpack (unVersion (packageVersion pkg))

maybeToList :: Maybe a -> [a]
maybeToList =
  maybe [] pure

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Hgs.Locals
  ( LocalPackage(..)
  , inspectLocals
  , renderLocals
  ) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Hgs.Domain
  ( Package(..)
  , PackageSource(..)
  , PlanGraph(..)
  , UnitId(..)
  )
import Hgs.PrettyPrint (renderPackage)

data LocalPackage = LocalPackage
  { localPackage       :: Package
  , localExternalDeps  :: [Package]
  }
  deriving stock (Eq, Show)

inspectLocals :: PlanGraph -> [LocalPackage]
inspectLocals graph =
  [ LocalPackage
      { localPackage = pkg
      , localExternalDeps = directExternalDeps pkg
      }
  | pkg <- Map.elems packages
  , packageSource pkg == PackageLocal
  ]
 where
  packages =
    planGraphPackages graph

  directExternalDeps pkg =
    [ depPkg
    | depUnitId <- Set.toAscList (packageDepends pkg)
    , depPkg <- maybeToList (Map.lookup depUnitId packages)
    , packageSource depPkg == PackageExternal
    ]

renderLocals :: [LocalPackage] -> String
renderLocals locals =
  case locals of
    [] ->
      "no local packages found\n"

    _ ->
      unlines $
        "local packages:"
          : concatMap renderLocalPackage locals

renderLocalPackage :: LocalPackage -> [String]
renderLocalPackage local =
  [ "  " <> renderPackage (localPackage local)
  , "    unit-id: " <> renderUnitId (localPackage local)
  , "    direct external deps:"
  ]
    <> renderDeps (localExternalDeps local)

renderUnitId :: Package -> String
renderUnitId =
  Text.unpack . unUnitId . packageUnitId

renderDeps :: [Package] -> [String]
renderDeps deps =
  case deps of
    [] ->
      ["      <none>"]

    _ ->
      map (("      " <>) . renderPackage) deps



maybeToList :: Maybe a -> [a]
maybeToList =
  maybe [] pure

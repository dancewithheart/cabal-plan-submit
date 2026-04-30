{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hgs.Extract
  ( extractPlanGraph
  , summarisePlanGraph
  ) where

import Data.Foldable (foldl')
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Hgs.Domain
  ( Package(..)
  , PackageName(..)
  , PackageSource(..)
  , PlanGraph(..)
  , RawPlan(..)
  , RawPlanItem(..)
  , UnitId(..)
  , Version(..)
  )
import Hgs.Domain (rawPkgSrcType)

extractPlanGraph :: RawPlan -> PlanGraph
extractPlanGraph rawPlan =
  PlanGraph
    { planGraphPackages = packages
    , planGraphLocals = localUnitIds
    }
 where
  completeItems :: [CompleteItem]
  completeItems =
    mapMaybe toCompleteItem (rawPlanItems rawPlan)

  knownUnitIds :: Set UnitId
  knownUnitIds =
    Set.fromList (map completeUnitId completeItems)

  localUnitIds :: Set UnitId
  localUnitIds =
    Set.fromList
      [ completeUnitId item
      | item <- completeItems
      , completeSource item == PackageLocal
      ]

  directExternalUnitIds :: Set UnitId
  directExternalUnitIds =
    Set.unions
      [ Set.filter (`Set.notMember` localUnitIds) (completeDepends item)
      | item <- completeItems
      , completeSource item == PackageLocal
      ]

  packages :: Map UnitId Package
  packages =
    Map.fromList
      [ ( completeUnitId item
        , Package
            { packageUnitId = completeUnitId item
            , packageName = completeName item
            , packageVersion = completeVersion item
            , packageSource = completeSource item
            , packageDepends = Set.filter (`Set.member` knownUnitIds) (completeDepends item)
            , packageIsDirect = completeUnitId item `Set.member` directExternalUnitIds
            }
        )
      | item <- completeItems
      ]

data CompleteItem = CompleteItem
  { completeUnitId  :: UnitId
  , completeName    :: PackageName
  , completeVersion :: Version
  , completeSource  :: PackageSource
  , completeDepends :: Set UnitId
  }

toCompleteItem :: RawPlanItem -> Maybe CompleteItem
toCompleteItem item = do
  unitId <- rawPlanItemId item
  name <- rawPlanItemPkgName item
  version <- rawPlanItemPkgVersion item
  pure
    CompleteItem
      { completeUnitId = unitId
      , completeName = name
      , completeVersion = version
      , completeSource = classifySource item
      , completeDepends = Set.fromList (rawPlanItemDepends item)
      }

classifySource :: RawPlanItem -> PackageSource
classifySource item =
  case rawPkgSrcTypeText item of
    Just "local" -> PackageLocal
    _            -> PackageExternal

rawPkgSrcTypeText :: RawPlanItem -> Maybe Text
rawPkgSrcTypeText =
  fmap rawPkgSrcTypeText' . rawPlanItemPkgSrc
 where
  rawPkgSrcTypeText' src = rawPkgSrcType src

summarisePlanGraph :: PlanGraph -> String
summarisePlanGraph graph =
  unlines $
    [ "packages-total:      " <> show totalCount
    , "packages-local:      " <> show localCount
    , "packages-external:   " <> show externalCount
    , "direct-external-deps:" <> show directExternalCount
    , "sample-packages:"
    ]
    <> map (("  - " <>) . summarisePackage) (take 8 packagesInOrder)
 where
  packagesInOrder = Map.elems (planGraphPackages graph)

  totalCount = length packagesInOrder

  localCount =
    countBy ((== PackageLocal) . packageSource) packagesInOrder

  externalCount =
    countBy ((== PackageExternal) . packageSource) packagesInOrder

  directExternalCount =
    countBy (\pkg -> packageSource pkg == PackageExternal && packageIsDirect pkg) packagesInOrder

summarisePackage :: Package -> String
summarisePackage pkg =
  intercalate
    ", "
    [ "id=" <> T.unpack (unUnitId (packageUnitId pkg))
    , "name=" <> T.unpack (unPackageName (packageName pkg))
    , "version=" <> T.unpack (unVersion (packageVersion pkg))
    , "source=" <> showSource (packageSource pkg)
    , "direct=" <> show (packageIsDirect pkg)
    , "deps=" <> show (Set.size (packageDepends pkg))
    ]

showSource :: PackageSource -> String
showSource = \case
  PackageLocal    -> "local"
  PackageExternal -> "external"

countBy :: (a -> Bool) -> [a] -> Int
countBy p = foldl' (\n a -> if p a then n + 1 else n) 0

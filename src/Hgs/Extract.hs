{-# LANGUAGE LambdaCase #-}

module Hgs.Extract
  ( extractPlanGraph
  , extractPlanGraphChecked
  , PlanGraphError(..)
  , summarisePlanGraph
  ) where

import Control.Monad (foldM, unless)
import Data.Foldable (foldl', traverse_)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Hgs.Domain
  ( ComponentName
  , DependencyKind(..)
  , DependencyOrigin(..)
  , Package(..)
  , PackageName(..)
  , PackageSource(..)
  , PlanGraph(..)
  , RawPkgSrc(rawPkgSrcKind, rawPkgSrcPath)
  , RawPlan(..)
  , RawPlanItem(..)
  , RawComponent(..)
  , UnitId(..)
  , Version(..)
  , packageDepends
  )

data PlanGraphError
  = IncompletePlanItem Int
  | DuplicateUnit UnitId
  | UnknownDependency UnitId UnitId
  deriving (Eq, Show)

-- Validate before extraction can discard incomplete items or unknown edges.
extractPlanGraphChecked :: RawPlan -> Either PlanGraphError PlanGraph
extractPlanGraphChecked plan = do
  items <- traverse complete (zip [0..] (rawPlanItems plan))
  known <- foldM addUnit Set.empty items
  traverse_ (checkDependencies known) items
  pure (extractCompleteItems items)
 where
  complete (index, item) =
    maybe (Left (IncompletePlanItem index)) Right (toCompleteItem item)

  addUnit known item
    | completeUnitId item `Set.member` known = Left (DuplicateUnit (completeUnitId item))
    | otherwise = Right (Set.insert (completeUnitId item) known)

  checkDependencies known item =
    traverse_ (\dependency ->
      unless (dependency `Set.member` known)
        (Left (UnknownDependency (completeUnitId item) dependency)))
      (Map.keys (completeDependencies item))

extractPlanGraph :: RawPlan -> Either PlanGraphError PlanGraph
extractPlanGraph = extractPlanGraphChecked

extractCompleteItems :: [CompleteItem] -> PlanGraph
extractCompleteItems completeItems =
  PlanGraph
    { planGraphPackages = packages
    , planGraphLocals = localUnitIds
    }
 where
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
      [ Set.filter (`Set.notMember` localUnitIds) (Map.keysSet (completeDependencies item))
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
            , packageSourcePath = completeSourcePath item
            , packageDependencies = completeDependencies item
            , packageIsDirect = completeUnitId item `Set.member` directExternalUnitIds
            }
        )
      | item <- completeItems
      ]

data CompleteItem = CompleteItem
  { completeUnitId     :: UnitId
  , completeName       :: PackageName
  , completeVersion    :: Version
  , completeSource     :: PackageSource
  , completeSourcePath :: Maybe FilePath
  , completeDependencies :: Map UnitId (Set DependencyOrigin)
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
      , completeSourcePath = rawPkgSrcPath =<< rawPlanItemPkgSrc item
      , completeDependencies = dependencyOrigins item
      }

dependencyOrigins :: RawPlanItem -> Map UnitId (Set DependencyOrigin)
dependencyOrigins item =
  Map.fromListWith Set.union $
    edges (rawPlanItemComponentName item) ComponentDependency (rawPlanItemDepends item)
      <> edges (rawPlanItemComponentName item) ExecutableDependency (rawPlanItemExeDepends item)
      <> concatMap componentEdges (rawPlanItemComponents item)
 where
  componentEdges component =
    let origin = Just (rawComponentName component)
     in edges origin ComponentDependency (rawComponentDepends component)
          <> edges origin ExecutableDependency (rawComponentExeDepends component)

edges :: Maybe ComponentName -> DependencyKind -> [UnitId] -> [(UnitId, Set DependencyOrigin)]
edges component kind =
  map (, Set.singleton (DependencyOrigin component kind))

classifySource :: RawPlanItem -> PackageSource
classifySource item =
  case rawPkgSrcKind =<< rawPlanItemPkgSrc item of
    Just t | t == Text.pack "local" -> PackageLocal
    _            -> PackageExternal

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
    [ "id=" <> Text.unpack (unUnitId (packageUnitId pkg))
    , "name=" <> Text.unpack (unPackageName (packageName pkg))
    , "version=" <> Text.unpack (unVersion (packageVersion pkg))
    , "source=" <> showSource (packageSource pkg)
    , "direct=" <> show (packageIsDirect pkg)
    , "deps=" <> show (Set.size (packageDepends pkg))
    ]

showSource :: PackageSource -> String
showSource = \case
  PackageLocal    -> "local"
  PackageExternal -> "external"

countBy :: (a -> Bool) -> [a] -> Int
countBy p = foldl' step 0
 where
  step n a
    | p a = n + 1
    | otherwise = n

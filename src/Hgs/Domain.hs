{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hgs.Domain
  ( PackageName(..)
  , Version(..)
  , UnitId(..)
  , RawPkgSrc(..)
  , RawPlan(..)
  , RawPlanItem(..)
  , PackageSource(..)
  , DependencyKind(..)
  , DependencyOrigin(..)
  , BuildConfiguration(..)
  , Package(..)
  , packageDepends
  , PlanGraph(..)
  , ComponentName(..)
  , RawComponent(..)
  ) where

import Data.Aeson
  ( FromJSON(parseJSON)
  , Value(..)
  , withObject
  , (.:)
  , (.:?)
  )
import Data.Aeson.Types (Parser)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap

import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text (Text)

newtype PackageName = PackageName { unPackageName :: Text }
  deriving stock (Eq, Ord, Show)

newtype Version = Version { unVersion :: Text }
  deriving stock (Eq, Ord, Show)

newtype UnitId = UnitId { unUnitId :: Text }
  deriving stock (Eq, Ord, Show)

data RawPkgSrc = RawPkgSrc
  { rawPkgSrcKind :: Maybe Text
  , rawPkgSrcPath :: Maybe FilePath
  }
  deriving stock (Eq, Show)

data RawPlan = RawPlan
  { rawPlanCabalVersion :: Maybe Text
  , rawPlanCompilerId   :: Maybe Text
  , rawPlanOS           :: Maybe Text
  , rawPlanArch         :: Maybe Text
  , rawPlanItems        :: [RawPlanItem]
  }
  deriving stock (Eq, Show)

data RawPlanItem = RawPlanItem
  { rawPlanItemType       :: Maybe Text
  , rawPlanItemId         :: Maybe UnitId
  , rawPlanItemPkgName    :: Maybe PackageName
  , rawPlanItemPkgVersion :: Maybe Version
  , rawPlanItemDepends    :: [UnitId]
  , rawPlanItemExeDepends :: [UnitId]
  , rawPlanItemComponents :: [RawComponent]
  , rawPlanItemComponentName :: Maybe ComponentName
  , rawPlanItemFlags      :: Maybe (Map Text Bool)
  , rawPlanItemPkgSrc     :: Maybe RawPkgSrc
  }
  deriving stock (Eq, Show)

data PackageSource
  = PackageLocal
  | PackageExternal
  deriving stock (Eq, Ord, Show)

-- These describe which Cabal plan field supplied the edge, not runtime scope.
data DependencyKind = ComponentDependency | ExecutableDependency
  deriving stock (Eq, Ord, Show)

data DependencyOrigin = DependencyOrigin
  { dependencyComponent :: Maybe ComponentName
  , dependencyKind :: DependencyKind
  }
  deriving stock (Eq, Ord, Show)

data BuildConfiguration = BuildConfiguration
  { configurationOS :: Maybe Text
  , configurationArch :: Maybe Text
  , configurationCompiler :: Maybe Text
  , configurationFlags :: Maybe (Map Text Bool)
  }
  deriving stock (Eq, Show)

data Package = Package
  { packageUnitId    :: UnitId
  , packageName      :: PackageName
  , packageVersion   :: Version
  , packageSource    :: PackageSource
  , packageSourcePath :: Maybe FilePath
  , packageComponentName :: Maybe ComponentName
  , packageConfiguration :: BuildConfiguration
  , packageDependencies :: Map UnitId (Set DependencyOrigin)
  , packageIsDirect  :: Bool
  }
  deriving stock (Eq, Show)

packageDepends :: Package -> Set UnitId
packageDepends = Map.keysSet . packageDependencies

data PlanGraph = PlanGraph
  { planGraphPackages :: Map UnitId Package
  , planGraphLocals   :: Set UnitId
  }
  deriving stock (Eq, Show)

instance FromJSON RawPlan where
  parseJSON = withObject "RawPlan" $ \o ->
    RawPlan
      <$> o .:? "cabal-lib-version"
      <*> o .:? "compiler-id"
      <*> o .:? "os"
      <*> o .:? "arch"
      <*> o .: "install-plan"

instance FromJSON RawPkgSrc where
  parseJSON = withObject "RawPkgSrc" $ \o ->
    RawPkgSrc
      <$> o .:? "type"
      <*> o .:? "path"

instance FromJSON RawPlanItem where
  parseJSON = withObject "RawPlanItem" $ \o -> do
    topDepends <-
      parseDependsField "depends" =<< o .:? "depends"
    topExeDepends <-
      parseDependsField "exe-depends" =<< o .:? "exe-depends"
    components <-
      parseComponents =<< o .:? "components"
    RawPlanItem
      <$> o .:? "type"
      <*> (fmap UnitId <$> o .:? "id")
      <*> (fmap PackageName <$> o .:? "pkg-name")
      <*> (fmap Version <$> o .:? "pkg-version")
      <*> pure topDepends
      <*> pure topExeDepends
      <*> pure components
      <*> (fmap ComponentName <$> o .:? "component-name")
      <*> o .:? "flags"
      <*> o .:? "pkg-src"

parseDependsField :: String -> Maybe Value -> Parser [UnitId]
parseDependsField field = \case
    Nothing -> pure []
    Just Null -> pure []
    Just (Array xs) -> traverse parseUnitIdValue (toList xs)
    Just _ -> fail (field <> ": expected array, null, or missing")

parseComponents :: Maybe Value -> Parser [RawComponent]
parseComponents = \case
    Nothing -> pure []
    Just Null -> pure []
    Just (Object components) -> traverse parseComponent (KeyMap.toList components)
    Just _ -> fail "components: expected object, null, or missing"

parseComponent :: (Key.Key, Value) -> Parser RawComponent
parseComponent (name, value) =
  case value of
    Object component -> do
      depends <-
        parseDependsField "depends" =<< component .:? "depends"
      exeDepends <-
        parseDependsField "exe-depends" =<< component .:? "exe-depends"
      pure
        RawComponent
          { rawComponentName = ComponentName (Key.toText name)
          , rawComponentDepends = depends
          , rawComponentExeDepends = exeDepends
          }
    _ -> fail "component: expected object"

parseUnitIdValue :: Value -> Parser UnitId
parseUnitIdValue = \case
    String t -> pure (UnitId t)
    _ -> fail "dependency id: expected string"

newtype ComponentName = ComponentName { unComponentName :: Text }
  deriving stock (Eq, Ord, Show)

data RawComponent = RawComponent
  { rawComponentName    :: ComponentName
  , rawComponentDepends :: [UnitId]
  , rawComponentExeDepends :: [UnitId]
  }
  deriving stock (Eq, Show)

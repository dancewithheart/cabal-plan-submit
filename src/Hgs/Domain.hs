{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Hgs.Domain
  ( PackageName(..)
  , Version(..)
  , UnitId(..)
  , RawPkgSrc(..)
  , RawPlan(..)
  , RawPlanItem(..)
  , PackageSource(..)
  , Package(..)
  , PlanGraph(..)
  ) where

import Data.Aeson
  ( FromJSON(parseJSON)
  , withObject
  , (.:?)
  , (.!=)
  )

import Data.Map.Strict (Map)
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
  , rawPlanItems        :: [RawPlanItem]
  }
  deriving stock (Eq, Show)

data RawPlanItem = RawPlanItem
  { rawPlanItemType       :: Maybe Text
  , rawPlanItemId         :: Maybe UnitId
  , rawPlanItemPkgName    :: Maybe PackageName
  , rawPlanItemPkgVersion :: Maybe Version
  , rawPlanItemDepends    :: [UnitId]
  , rawPlanItemPkgSrc     :: Maybe RawPkgSrc
  }
  deriving stock (Eq, Show)

data PackageSource
  = PackageLocal
  | PackageExternal
  deriving stock (Eq, Ord, Show)

data Package = Package
  { packageUnitId   :: UnitId
  , packageName     :: PackageName
  , packageVersion  :: Version
  , packageSource   :: PackageSource
  , packageDepends  :: Set UnitId
  , packageIsDirect :: Bool
  }
  deriving stock (Eq, Show)

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
      <*> o .:? "install-plan" .!= []

instance FromJSON RawPkgSrc where
  parseJSON = withObject "RawPkgSrc" $ \o ->
    RawPkgSrc
      <$> o .:? "type"
      <*> o .:? "path"

instance FromJSON RawPlanItem where
  parseJSON = withObject "RawPlanItem" $ \o ->
    RawPlanItem
      <$> o .:? "type"
      <*> (fmap UnitId <$> o .:? "id")
      <*> (fmap PackageName <$> o .:? "pkg-name")
      <*> (fmap Version <$> o .:? "pkg-version")
      <*> (fmap UnitId <$> (o .:? "depends" .!= []))
      <*> o .:? "pkg-src"


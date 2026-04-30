{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hgs.Domain
  ( PackageName(..)
  , Version(..)
  , UnitId(..)
  , RawPlan(..)
  , RawPlanItem(..)
  ) where

import Data.Text (Text)

newtype PackageName = PackageName { unPackageName :: Text }
  deriving stock (Eq, Ord, Show)

newtype Version = Version { unVersion :: Text }
  deriving stock (Eq, Ord, Show)

newtype UnitId = UnitId { unUnitId :: Text }
  deriving stock (Eq, Ord, Show)

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
  }
  deriving stock (Eq, Show)

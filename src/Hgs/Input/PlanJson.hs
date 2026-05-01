{-# LANGUAGE OverloadedStrings #-}

module Hgs.Input.PlanJson
  ( readRawPlan
  , decodeRawPlan
  , summariseRawPlan
  , summariseRawPlanItem
  ) where

import Data.Aeson (eitherDecodeStrict')
import Data.ByteString qualified as BS
import Data.List (intercalate)
import Data.Text (Text)
import Data.Text qualified as Text
import Hgs.Domain
  ( PackageName(..)
  , RawPkgSrc(..)
  , RawPlan(..)
  , RawPlanItem(..)
  , UnitId(..)
  , Version(..)
  )

readRawPlan :: FilePath -> IO (Either String RawPlan)
readRawPlan = fmap decodeRawPlan . BS.readFile

decodeRawPlan :: BS.ByteString -> Either String RawPlan
decodeRawPlan = eitherDecodeStrict'

summariseRawPlan :: RawPlan -> String
summariseRawPlan plan =
  unlines $
    [ "cabal-lib-version: " <> renderMaybeText (rawPlanCabalVersion plan)
    , "compiler-id:       " <> renderMaybeText (rawPlanCompilerId plan)
    , "install-plan-size: " <> show (length (rawPlanItems plan))
    , "sample-items:"
    ]
      <> map (("  - " <>) . summariseRawPlanItem) (take 5 (rawPlanItems plan))

summariseRawPlanItem :: RawPlanItem -> String
summariseRawPlanItem item =
  intercalate
    ", "
    [ "type=" <> renderMaybeText (rawPlanItemType item)
    , "id=" <> renderMaybeText (unUnitId <$> rawPlanItemId item)
    , "name=" <> renderMaybeText (unPackageName <$> rawPlanItemPkgName item)
    , "version=" <> renderMaybeText (unVersion <$> rawPlanItemPkgVersion item)
    , "deps=" <> show (length (rawPlanItemDepends item))
    , "pkg-src=" <> renderMaybeText (rawPkgSrcKind =<< rawPlanItemPkgSrc item)
    ]

renderMaybeText :: Maybe Text -> String
renderMaybeText = maybe "<missing>" Text.unpack

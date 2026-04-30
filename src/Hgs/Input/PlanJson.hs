{-# LANGUAGE OverloadedStrings #-}

module Hgs.Input.PlanJson
  ( readRawPlan
  , decodeRawPlan
  , summariseRawPlan
  , summariseRawPlanItem
  ) where

import Data.Aeson
  ( FromJSON(parseJSON)
  , eitherDecodeStrict'
  , withObject
  , (.:?)
  , (.!=)
  )
import qualified Data.ByteString as BS
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Hgs.Domain
  ( PackageName(..)
  , RawPlan(..)
  , RawPlanItem(..)
  , UnitId(..)
  , Version(..)
  )

readRawPlan :: FilePath -> IO (Either String RawPlan)
readRawPlan = fmap decodeRawPlan . BS.readFile

decodeRawPlan :: BS.ByteString -> Either String RawPlan
decodeRawPlan = eitherDecodeStrict'

instance FromJSON RawPlan where
  parseJSON = withObject "RawPlan" $ \o ->
    RawPlan
      <$> o .:? "cabal-lib-version"
      <*> o .:? "compiler-id"
      <*> o .:? "install-plan" .!= []

instance FromJSON RawPlanItem where
  parseJSON = withObject "RawPlanItem" $ \o ->
    RawPlanItem
      <$> o .:? "type"
      <*> (fmap UnitId <$> o .:? "id")
      <*> (fmap PackageName <$> o .:? "pkg-name")
      <*> (fmap Version <$> o .:? "pkg-version")
      <*> (fmap UnitId <$> (o .:? "depends" .!= []))

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
    [ "type="    <> renderMaybeText (rawPlanItemType item)
    , "id="      <> renderMaybeText (unUnitId <$> rawPlanItemId item)
    , "name="    <> renderMaybeText (unPackageName <$> rawPlanItemPkgName item)
    , "version=" <> renderMaybeText (unVersion <$> rawPlanItemPkgVersion item)
    , "deps="    <> show (length (rawPlanItemDepends item))
    ]

renderMaybeText :: Maybe Text -> String
renderMaybeText = maybe "<missing>" T.unpack

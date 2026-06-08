{-# LANGUAGE OverloadedStrings #-}

module Hgs.PrettyPrint (renderPackage, renderPackageText) where

import Hgs.Domain
  ( Package(..)
  , PackageName(..)
  , Version(..)
  )

import Data.Text (Text)
import Data.Text qualified as Text

renderPackage :: Package -> String
renderPackage =
  Text.unpack . renderPackageText

renderPackageText :: Package -> Text
renderPackageText pkg =
  renderPackageName (packageName pkg)
    <> "-"
    <> renderVersion (packageVersion pkg)

renderPackageName :: PackageName -> Text
renderPackageName =
  unPackageName

renderVersion :: Version -> Text
renderVersion =
  unVersion

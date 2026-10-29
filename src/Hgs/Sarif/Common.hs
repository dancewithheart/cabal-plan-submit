{-# LANGUAGE OverloadedStrings #-}

module Hgs.Sarif.Common
  ( localRootAndDirectDependency
  , locationValue
  ) where

import Data.Aeson (Value(..))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text

import Hgs.Domain (Package)
import Hgs.Paths (PackagePath(..))

localRootAndDirectDependency :: PackagePath -> Maybe (Package, Package)
localRootAndDirectDependency path =
  case unPackagePath path of
    root : directDep : _ -> Just (root, directDep)
    _                    -> Nothing

locationValue :: FilePath -> Maybe Int -> Value
locationValue cabalFile maybeLine =
  Object $
    KeyMap.fromList
      [ ( "physicalLocation"
        , Object $
            KeyMap.fromList
              [ ( "artifactLocation"
                , Object $
                    KeyMap.fromList
                      [ ("uri", String (Text.pack cabalFile))
                      ]
                )
              , ( "region"
                , Object $
                    KeyMap.fromList
                      [ ("startLine", Aeson.toJSON (fromMaybe 1 maybeLine))
                      , ("startColumn", Aeson.toJSON (1 :: Int))
                      ]
                )
              ]
        )
      ]

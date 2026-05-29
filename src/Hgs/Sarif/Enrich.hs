{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hgs.Sarif.Enrich
  ( enrichSarifValue
  ) where

import Data.Aeson (Value(..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down(..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Hgs.Domain
  ( Package(..)
  , PackageName(..)
  , PackageSource(..)
  , PlanGraph(..)
  , Version(..)
  )
import Hgs.LocalUnitFilter
  ( LocalUnitFilter
  )
import Hgs.Why
  ( PackagePath(..)
  , renderPackagePath
  , shortestPathsToPackageFrom
  )

data FindingExplanation = FindingExplanation
  { explainedPackage      :: Package
  , explainedRelationship :: Text
  , explainedPaths        :: [PackagePath]
  }
  deriving stock (Eq, Show)

enrichSarifValue :: LocalUnitFilter -> PlanGraph -> Value -> Value
enrichSarifValue localFilter graph =
  \case
    Object o ->
      Object (adjustArrayField "runs" (enrichRun localFilter graph) o)

    other ->
      other

enrichRun :: LocalUnitFilter -> PlanGraph -> Value -> Value
enrichRun localFilter graph =
  \case
    Object o ->
      Object (adjustArrayField "results" (enrichResult localFilter graph) o)

    other ->
      other

enrichResult :: LocalUnitFilter -> PlanGraph -> Value -> Value
enrichResult localFilter graph result =
  case result of
    Object o ->
      case explainResult localFilter graph result of
        Nothing ->
          result

        Just explanation ->
          Object (addExplanation explanation o)

    _ ->
      result

explainResult :: LocalUnitFilter -> PlanGraph -> Value -> Maybe FindingExplanation
explainResult localFilter graph result = do
  pkg <- findMentionedPackage graph result
  let paths =
        [ path
        | path <- shortestPathsToPackageFrom localFilter (packageName pkg) graph
        , pathEndsAt pkg path
        ]
  case paths of
    [] ->
      Nothing

    _ ->
      Just
        FindingExplanation
          { explainedPackage = pkg
          , explainedRelationship = relationshipFromPaths paths
          , explainedPaths = paths
          }

findMentionedPackage :: PlanGraph -> Value -> Maybe Package
findMentionedPackage graph result =
  firstJust
    [ exactVersionMatch
    , uniqueNameMatch
    ]
 where
  haystack =
    Text.unwords (collectStrings result)

  candidatePackages =
    sortOn
      (Down . Text.length . unPackageName . packageName)
      [ pkg
      | pkg <- Map.elems (planGraphPackages graph)
      , packageSource pkg == PackageExternal
      ]

  exactVersionMatch =
    firstJust
      [ Just pkg
      | pkg <- candidatePackages
      , packageNameMentioned pkg haystack
      , packageVersionMentioned pkg haystack
      ]

  uniqueNameMatch =
    case
      [ pkg
      | pkg <- candidatePackages
      , packageNameMentioned pkg haystack
      ]
    of
      [pkg] -> Just pkg
      _     -> Nothing

packageNameMentioned :: Package -> Text -> Bool
packageNameMentioned pkg haystack =
  unPackageName (packageName pkg) `Text.isInfixOf` haystack

packageVersionMentioned :: Package -> Text -> Bool
packageVersionMentioned pkg haystack =
  unVersion (packageVersion pkg) `Text.isInfixOf` haystack

pathEndsAt :: Package -> PackagePath -> Bool
pathEndsAt pkg path =
  case reverse (unPackagePath path) of
    target : _ ->
      packageName target == packageName pkg
        && packageVersion target == packageVersion pkg

    [] ->
      False

relationshipFromPaths :: [PackagePath] -> Text
relationshipFromPaths paths
  | any isDirectPath paths = "direct"
  | otherwise = "indirect"
 where
  isDirectPath path =
    case unPackagePath path of
      [_localRoot, _target] ->
        True
      _ ->
        False

addExplanation :: FindingExplanation -> KeyMap Value -> KeyMap Value
addExplanation explanation =
  addProperties explanation
    . addMessageExplanation explanation

addMessageExplanation :: FindingExplanation -> KeyMap Value -> KeyMap Value
addMessageExplanation explanation o =
  KeyMap.insert "message" (Object message') o
 where
  oldMessage =
    case KeyMap.lookup "message" o of
      Just (Object msg) -> msg
      _ -> KeyMap.empty

  oldText =
    case KeyMap.lookup "text" oldMessage of
      Just (String t) -> t
      _ -> ""

  message' =
    KeyMap.insert
      "text"
      (String (appendExplanationText oldText explanation))
      oldMessage

appendExplanationText :: Text -> FindingExplanation -> Text
appendExplanationText oldText explanation =
  Text.stripEnd oldText
    <> "\n\ncabal-plan-submit:\n"
    <> "  package: "
    <> renderPackageText (explainedPackage explanation)
    <> "\n"
    <> "  relationship: "
    <> explainedRelationship explanation
    <> "\n"
    <> "  paths:\n"
    <> Text.concat
      [ "    - " <> Text.pack (renderPackagePath path) <> "\n"
      | path <- explainedPaths explanation
      ]

addProperties :: FindingExplanation -> KeyMap Value -> KeyMap Value
addProperties explanation o =
  KeyMap.insert "properties" (Object properties') o
 where
  oldProperties =
    case KeyMap.lookup "properties" o of
      Just (Object p) -> p
      _ -> KeyMap.empty

  properties' =
    KeyMap.union
      (KeyMap.fromList
        [ ("cabal-plan-submit.package", String (renderPackageText (explainedPackage explanation)))
        , ("cabal-plan-submit.relationship", String (explainedRelationship explanation))
        , ( "cabal-plan-submit.paths"
          , Aeson.toJSON
              [ Text.pack (renderPackagePath path)
              | path <- explainedPaths explanation
              ]
          )
        ])
      oldProperties

renderPackageText :: Package -> Text
renderPackageText pkg =
  unPackageName (packageName pkg)
    <> "-"
    <> unVersion (packageVersion pkg)

adjustArrayField :: Text -> (Value -> Value) -> KeyMap Value -> KeyMap Value
adjustArrayField field f o =
  case KeyMap.lookup (Key.fromText field) o of
    Just (Array xs) ->
      KeyMap.insert (Key.fromText field) (Array (Vector.map f xs)) o

    _ ->
      o

collectStrings :: Value -> [Text]
collectStrings =
  \case
    Object o -> concatMap collectStrings (KeyMap.elems o)
    Array xs -> concatMap collectStrings (Vector.toList xs)
    String t -> [t]
    Number _ -> []
    Bool _ -> []
    Null ->  []

firstJust :: [Maybe a] -> Maybe a
firstJust =
  \case
    [] -> Nothing
    Nothing : xs -> firstJust xs
    Just x : _ -> Just x

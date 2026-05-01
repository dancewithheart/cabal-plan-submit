{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hgs.Deprecated
  ( Deprecation(..)
  , DeprecatedPackage(..)
  , readDeprecationIndex
  , findDeprecatedPackages
  , renderDeprecatedPackages
  ) where

import Data.Aeson (Value(..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Data.Yaml qualified as Yaml
import Hgs.Domain
  ( Package(..)
  , PackageName(..)
  , PackageSource(..)
  , PlanGraph(..)
  , Version(..)
  )
import Hgs.Why
  ( PackagePath
  , renderPackagePath
  , shortestPathsToPackage
  )

data Deprecation = Deprecation
  { deprecationPackage      :: PackageName
  , deprecationReplacements :: [PackageName]
  , deprecationReason       :: Maybe Text
  }
  deriving stock (Eq, Show)

data DeprecatedPackage = DeprecatedPackage
  { deprecatedPackageName    :: PackageName
  , deprecatedPackageVersion :: Version
  , deprecatedRelationship   :: Text
  , deprecatedReplacements   :: [PackageName]
  , deprecatedReason         :: Maybe Text
  , deprecatedPath           :: Maybe PackagePath
  }
  deriving stock (Eq, Show)

readDeprecationIndex :: FilePath -> IO (Either String (Map PackageName Deprecation))
readDeprecationIndex path = do
  result <- Yaml.decodeFileEither path
  pure $
    case result of
      Left err ->
        Left (Yaml.prettyPrintParseException err)
      Right value ->
        deprecationIndexFromValue value

deprecationIndexFromValue :: Value -> Either String (Map PackageName Deprecation)
deprecationIndexFromValue =
  \case
    Array xs ->
      Right $
        Map.fromList
          [ (deprecationPackage dep, dep)
          | dep <- mapMaybe deprecationEntryFromValue (Vector.toList xs)
          ]

    Object o ->
      Right $
        Map.fromList
          [ (pkgName, dep)
          | (key, value) <- KeyMap.toList o
          , let pkgName = PackageName (Key.toText key)
          , dep <- maybeToList (deprecationFromValue pkgName value)
          ]

    _ ->
      Left "deprecated.yaml: expected top-level YAML array or object"

deprecationEntryFromValue :: Value -> Maybe Deprecation
deprecationEntryFromValue =
  \case
    Object o -> do
      pkgText <- textField "deprecated-package" o
      let pkgName = PackageName pkgText
      pure $
        Deprecation
          { deprecationPackage = pkgName
          , deprecationReplacements =
              replacementsFromObject o
          , deprecationReason =
              reasonFromObject o
          }

    _ ->
      Nothing

deprecationFromValue :: PackageName -> Value -> Maybe Deprecation
deprecationFromValue pkgName value =
  case value of
    Bool False ->
      Nothing

    Null ->
      Just (Deprecation pkgName [] Nothing)

    Bool True ->
      Just (Deprecation pkgName [] Nothing)

    String t ->
      Just $
        Deprecation
          { deprecationPackage = pkgName
          , deprecationReplacements =
              maybeToList (replacementFromText t)
          , deprecationReason = Nothing
          }

    Array xs ->
      Just $
        Deprecation
          { deprecationPackage = pkgName
          , deprecationReplacements =
              PackageName <$> replacementTextsFromValues (Vector.toList xs)
          , deprecationReason = Nothing
          }

    Object o ->
      Just $
        Deprecation
          { deprecationPackage = pkgName
          , deprecationReplacements = replacementsFromObject o
          , deprecationReason = reasonFromObject o
          }

    Number _ ->
      Nothing

replacementsFromObject :: KeyMap.KeyMap Value -> [PackageName]
replacementsFromObject o =
  PackageName <$>
    nub
      ( concat
          [ maybeToList (textField "replacement" o)
          , maybeToList (textField "replaced-by" o)
          , maybeToList (textField "replaced_by" o)
          , maybeToList (textField "in-favour-of" o)
          , maybeToList (textField "in-favor-of" o)
          , maybe [] replacementTextsFromValues (arrayField "replacement" o)
          , maybe [] replacementTextsFromValues (arrayField "replacements" o)
          , maybe [] replacementTextsFromValues (arrayField "replaced-by" o)
          , maybe [] replacementTextsFromValues (arrayField "in-favour-of" o)
          , maybe [] replacementTextsFromValues (arrayField "in-favor-of" o)
          ]
      )

reasonFromObject :: KeyMap.KeyMap Value -> Maybe Text
reasonFromObject o =
  firstJust
    [ textField "reason" o
    , textField "message" o
    , textField "description" o
    ]

replacementFromText :: Text -> Maybe PackageName
replacementFromText t
  | isReplacementLike t = Just (PackageName (Text.strip t))
  | otherwise = Nothing

replacementTextsFromValues :: [Value] -> [Text]
replacementTextsFromValues =
  nub . filter isReplacementLike . mapMaybe valueText

isReplacementLike :: Text -> Bool
isReplacementLike t =
  let x = Text.strip t
   in not (Text.null x)
        && x `notElem` ["deprecated", "true", "yes"]

valueText :: Value -> Maybe Text
valueText =
  \case
    String t -> Just t
    _ -> Nothing

textField :: Text -> KeyMap.KeyMap Value -> Maybe Text
textField key o =
  case KeyMap.lookup (Key.fromText key) o of
    Just (String t) -> Just t
    _ -> Nothing

arrayField :: Text -> KeyMap.KeyMap Value -> Maybe [Value]
arrayField key o =
  case KeyMap.lookup (Key.fromText key) o of
    Just (Array xs) -> Just (Vector.toList xs)
    _ -> Nothing

findDeprecatedPackages :: Map PackageName Deprecation -> PlanGraph -> [DeprecatedPackage]
findDeprecatedPackages index graph =
  Map.elems $
    Map.fromList
      [ ( (packageName pkg, packageVersion pkg)
        , DeprecatedPackage
            { deprecatedPackageName = packageName pkg
            , deprecatedPackageVersion = packageVersion pkg
            , deprecatedRelationship =
                if packageIsDirect pkg then "direct" else "indirect"
            , deprecatedReplacements =
                deprecationReplacements dep
            , deprecatedReason =
                deprecationReason dep
            , deprecatedPath =
                listToMaybe (shortestPathsToPackage (packageName pkg) graph)
            }
        )
      | pkg <- Map.elems (planGraphPackages graph)
      , packageSource pkg == PackageExternal
      , dep <- maybeToList (Map.lookup (packageName pkg) index)
      ]

renderDeprecatedPackages :: [DeprecatedPackage] -> String
renderDeprecatedPackages deps =
  case deps of
    [] ->
      "no deprecated packages found\n"

    _ ->
      unlines $
        "deprecated packages:"
          : concatMap renderDeprecatedPackage deps

renderDeprecatedPackage :: DeprecatedPackage -> [String]
renderDeprecatedPackage dep =
  [ "  " <> packageNameText (deprecatedPackageName dep) <> "-" <> versionText (deprecatedPackageVersion dep)
  , "    relationship: " <> Text.unpack (deprecatedRelationship dep)
  ]
    <> renderReplacements (deprecatedReplacements dep)
    <> maybe [] (\r -> ["    reason: " <> Text.unpack r]) (deprecatedReason dep)
    <> maybe [] (\path -> ["    used by path: " <> renderPackagePath path]) (deprecatedPath dep)

renderReplacements :: [PackageName] -> [String]
renderReplacements replacements =
  case replacements of
    [] ->
      ["    replacements: <none>"]

    [one] ->
      ["    replacement: " <> packageNameText one]

    many ->
      "    replacements:"
        : map (\pkg -> "      - " <> packageNameText pkg) many

packageNameText :: PackageName -> String
packageNameText =
  Text.unpack . unPackageName

versionText :: Version -> String
versionText =
  Text.unpack . unVersion

firstJust :: [Maybe a] -> Maybe a
firstJust =
  listToMaybe . catMaybes

maybeToList :: Maybe a -> [a]
maybeToList =
  maybe [] pure

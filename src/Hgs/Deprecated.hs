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

data Deprecation = Deprecation
  { deprecationPackage     :: PackageName
  , deprecationReplacement :: Maybe PackageName
  , deprecationReason      :: Maybe Text
  }
  deriving stock (Eq, Show)

data DeprecatedPackage = DeprecatedPackage
  { deprecatedPackageName    :: PackageName
  , deprecatedPackageVersion :: Version
  , deprecatedRelationship   :: Text
  , deprecatedReplacement    :: Maybe PackageName
  , deprecatedReason         :: Maybe Text
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
          , deprecationReplacement =
              replacementFromObject o
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
      Just (Deprecation pkgName Nothing Nothing)

    Bool True ->
      Just (Deprecation pkgName Nothing Nothing)

    String t ->
      Just $
        Deprecation
          { deprecationPackage = pkgName
          , deprecationReplacement = replacementFromText t
          , deprecationReason = reasonFromText t
          }

    Array xs ->
      Just $
        Deprecation
          { deprecationPackage = pkgName
          , deprecationReplacement =
              PackageName <$> firstReplacementText (Vector.toList xs)
          , deprecationReason = Nothing
          }

    Object o ->
      Just $
        Deprecation
          { deprecationPackage = pkgName
          , deprecationReplacement = replacementFromObject o
          , deprecationReason = reasonFromObject o
          }

    Number _ ->
      Nothing

replacementFromObject :: KeyMap.KeyMap Value -> Maybe PackageName
replacementFromObject o =
  PackageName <$>
    firstJust
      [ textField "replacement" o
      , textField "replaced-by" o
      , textField "replaced_by" o
      , textField "in-favour-of" o
      , textField "in-favor-of" o
      , firstReplacementText =<< arrayField "replacement" o
      , firstReplacementText =<< arrayField "replacements" o
      , firstReplacementText =<< arrayField "replaced-by" o
      , firstReplacementText =<< arrayField "in-favour-of" o
      , firstReplacementText =<< arrayField "in-favor-of" o
      ]

reasonFromObject :: KeyMap.KeyMap Value -> Maybe Text
reasonFromObject o =
  firstJust
    [ textField "reason" o
    , textField "message" o
    , textField "description" o
    ]

replacementFromText :: Text -> Maybe PackageName
replacementFromText t
  | normalized `elem` ["deprecated", "true", "yes"] =
      Nothing
  | Text.null normalized =
      Nothing
  | otherwise =
      Just (PackageName normalized)
 where
  normalized =
    Text.strip t

reasonFromText :: Text -> Maybe Text
reasonFromText t
  | replacementFromText t == Nothing = Nothing
  | otherwise = Nothing

firstReplacementText :: [Value] -> Maybe Text
firstReplacementText =
  listToMaybe
    . filter isReplacementLike
    . mapMaybe valueText

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
            , deprecatedReplacement =
                deprecationReplacement dep
            , deprecatedReason =
                deprecationReason dep
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
  , "    replacement: " <> maybe "<none>" packageNameText (deprecatedReplacement dep)
  ]
    <> maybe [] (\r -> ["    reason: " <> Text.unpack r]) (deprecatedReason dep)

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

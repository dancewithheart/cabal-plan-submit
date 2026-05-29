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
import Data.Set qualified as Set
import System.FilePath ((</>), takeExtension)
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
    [ packageFromConcernedNames
    , exactVersionMatch
    ]
 where
  haystack =
    Text.unwords (collectStrings result)

  concernedNames =
    extractConcernedPackageNames haystack

  candidatePackages =
    sortOn
      (Down . Text.length . unPackageName . packageName)
      [ pkg
      | pkg <- Map.elems (planGraphPackages graph)
      , packageSource pkg == PackageExternal
      ]

  packageFromConcernedNames =
    firstJust
      [ Just pkg
      | name <- concernedNames
      , pkg <- candidatePackages
      , unPackageName (packageName pkg) == name
      ]

  exactVersionMatch =
    firstJust
      [ Just pkg
      | pkg <- candidatePackages
      , exactPackageNameMentioned pkg haystack
      , unVersion (packageVersion pkg) `Text.isInfixOf` haystack
      ]

extractConcernedPackageNames :: Text -> [Text]
extractConcernedPackageNames haystack =
  dedupeText $
    concatMap namesFromParenthesizedLine (Text.lines haystack)

namesFromParenthesizedLine :: Text -> [Text]
namesFromParenthesizedLine line =
  case Text.stripPrefix "(" (Text.strip line) of
    Nothing ->
      []

    Just rest ->
      case Text.breakOn ")" rest of
        (inside, after)
          | ")" `Text.isPrefixOf` after ->
              filter plausiblePackageName $
                map Text.strip $
                  Text.splitOn "," inside
          | otherwise ->
              []

exactPackageNameMentioned :: Package -> Text -> Bool
exactPackageNameMentioned pkg haystack =
  unPackageName (packageName pkg) `elem` extractConcernedPackageNames haystack

plausiblePackageName :: Text -> Bool
plausiblePackageName name =
  not (Text.null name)
    && Text.all validChar name
 where
  validChar c =
    ('a' <= c && c <= 'z')
      || ('A' <= c && c <= 'Z')
      || ('0' <= c && c <= '9')
      || c == '-'
      || c == '_'

dedupeText :: [Text] -> [Text]
dedupeText =
  go Set.empty
 where
  go _ [] =
    []

  go seen (x : xs)
    | x `Set.member` seen =
        go seen xs
    | otherwise =
        x : go (Set.insert x seen) xs

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
  addLevel explanation
    . addLocations explanation
    . addProperties explanation
    . addMessageExplanation explanation

addLevel :: FindingExplanation -> KeyMap Value -> KeyMap Value
addLevel explanation o =
  case explainedRelationship explanation of
    "direct" ->
      KeyMap.insert "level" (String "error") o

    "indirect" ->
      KeyMap.insert "level" (String "warning") o

    _ ->
      o

addLocations :: FindingExplanation -> KeyMap Value -> KeyMap Value
addLocations explanation o =
  case explanationLocations explanation of
    [] ->
      o

    locations ->
      KeyMap.insert "locations" (Array (Vector.fromList locations)) o

explanationLocations :: FindingExplanation -> [Value]
explanationLocations explanation =
  [ locationValue root path
  | root <- uniqueLocalRoots (explainedPaths explanation)
  , path <- maybeToList (packageSourcePath root)
  ]

uniqueLocalRoots :: [PackagePath] -> [Package]
uniqueLocalRoots paths =
  nubOn packageKey
    [ root
    | PackagePath (root : _) <- paths
    , packageSource root == PackageLocal
    ]

packageKey :: Package -> (PackageName, Version)
packageKey pkg =
  (packageName pkg, packageVersion pkg)

locationValue :: Package -> FilePath -> Value
locationValue root path =
  Object $
    KeyMap.fromList
      [ ( "physicalLocation"
        , Object $
            KeyMap.fromList
              [ ( "artifactLocation"
                , Object $
                    KeyMap.fromList
                      [ ("uri", String (Text.pack (normaliseFileUri (localPackageCabalFile root path))))
                      ]
                )
              , ( "region"
                , Object $
                    KeyMap.fromList
                      [ ("startLine", Aeson.toJSON (1 :: Int))
                      , ("startColumn", Aeson.toJSON (1 :: Int))
                      ]
                )
              ]
        )
      ]

localPackageCabalFile :: Package -> FilePath -> FilePath
localPackageCabalFile pkg path
  | takeExtension path == ".cabal" =
      path

  | otherwise =
      path </> Text.unpack (unPackageName (packageName pkg)) <> ".cabal"

normaliseFileUri :: FilePath -> String
normaliseFileUri path
  | "file://" `prefixOf` path = path
  | "/" `prefixOf` path = "file://" <> path
  | otherwise = path

prefixOf :: String -> String -> Bool
prefixOf prefix text =
  take (length prefix) text == prefix

nubOn :: Ord b => (a -> b) -> [a] -> [a]
nubOn f =
  go Set.empty
 where
  go _ [] =
    []

  go seen (x : xs)
    | key `Set.member` seen =
        go seen xs
    | otherwise =
        x : go (Set.insert key seen) xs
   where
    key =
      f x

maybeToList :: Maybe a -> [a]
maybeToList =
  maybe [] pure

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

  oldMarkdown =
    case KeyMap.lookup "markdown" oldMessage of
      Just (String t) -> t
      _ -> ""

  message' =
    KeyMap.insert "markdown" (String (appendExplanationMarkdown oldMarkdown explanation)) $
      KeyMap.insert "text" (String (appendExplanationText oldText explanation)) oldMessage

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

appendExplanationMarkdown :: Text -> FindingExplanation -> Text
appendExplanationMarkdown oldMarkdown explanation =
  Text.stripEnd oldMarkdown
    <> "\n\n## cabal-plan-submit dependency path\n\n"
    <> "* package: `"
    <> renderPackageText (explainedPackage explanation)
    <> "`\n"
    <> "* relationship: `"
    <> explainedRelationship explanation
    <> "`\n"
    <> "* paths:\n"
    <> Text.concat
      [ "  * `"
          <> Text.pack (renderPackagePath path)
          <> "`\n"
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
        , ("tags", Aeson.toJSON (resultTags explanation))
        , ("precision", String "medium")
        , ("problem.severity", String (problemSeverity explanation))
        ])
      oldProperties

resultTags :: FindingExplanation -> [Text]
resultTags explanation =
  [ "haskell"
  , "cabal"
  , "dependency"
  , "cabal-plan-submit"
  , case explainedRelationship explanation of
      "direct" -> "direct-dependency"
      _        -> "transitive-dependency"
  ]

problemSeverity :: FindingExplanation -> Text
problemSeverity explanation =
  case explainedRelationship explanation of
    "direct" ->
      "error"

    "indirect" ->
      "warning"

    _ ->
      "recommendation"

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
    Null -> []

firstJust :: [Maybe a] -> Maybe a
firstJust =
  \case
    [] -> Nothing
    Nothing : xs -> firstJust xs
    Just x : _ -> Just x

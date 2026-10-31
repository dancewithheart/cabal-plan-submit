{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hgs.Sarif.Enrich
  ( CabalLineIndex
  , enrichSarifValue
  ) where

import Data.Aeson (Value(..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Foldable (asum)
import Data.Set qualified as Set
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, maybeToList)
import System.FilePath
  ( dropTrailingPathSeparator
  , normalise
  )
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Hgs.Domain
  ( Package(..)
  , PackageName(..)
  , PackageSource(..)
  , PlanGraph(..)
  )
import Hgs.LocalUnitFilter
  ( LocalUnitFilter
  )
import Hgs.Paths
  ( PackagePath(..)
  , renderPackagePath
  )
import Hgs.Why (shortestPathsToPackageFrom)
import Hgs.PrettyPrint (renderPackageText)
import Hgs.CabalFileIndex
  ( CabalLineIndex
  , hasIndexedCabalFile
  , lookupDependencyLine
  , localPackageCabalFile
  , repoRelativeCabalFile
  )
import Hgs.Sarif.Common
  ( localRootAndDirectDependency
  , locationValue
  )


data FindingExplanation = FindingExplanation
  { explainedPackage      :: Package
  , explainedRelationship :: Text
  , explainedPaths        :: [PackagePath]
  }
  deriving stock (Eq, Show)

data PackageMatch
  = NoPackageMention
  | MatchedPackage Package
  | AmbiguousPackageMention PackageName [Package]
  deriving stock (Eq, Show)

enrichSarifValue :: CabalLineIndex -> LocalUnitFilter -> PlanGraph -> Value -> Value
enrichSarifValue lineIndex localFilter graph =
  \case
    Object o ->
      Object (adjustArrayField "runs" (enrichRun lineIndex localFilter graph) o)

    other ->
      other

enrichRun :: CabalLineIndex -> LocalUnitFilter -> PlanGraph -> Value -> Value
enrichRun lineIndex localFilter graph =
  \case
    Object o ->
      let repoRoot =
            fromMaybe
              (guessRepoRoot graph)
              (sarifRunRoot o)
       in Object (adjustArrayField "results" (enrichResult lineIndex repoRoot localFilter graph) o)

    other ->
      other

enrichResult :: CabalLineIndex -> FilePath -> LocalUnitFilter -> PlanGraph -> Value -> Value
enrichResult lineIndex repoRoot localFilter graph result =
  case result of
    Object o ->
      case explainResult localFilter graph result of
        Nothing ->
          Object o

        Just explanation ->
          Object (addExplanation lineIndex repoRoot explanation o)

    _ ->
      result

explainResult :: LocalUnitFilter -> PlanGraph -> Value -> Maybe FindingExplanation
explainResult localFilter graph result =
  case findMentionedPackage graph result of
    NoPackageMention -> Nothing
    AmbiguousPackageMention _ _ -> Nothing
    MatchedPackage pkg ->
      let paths =
            [ path
            | path <- shortestPathsToPackageFrom localFilter (packageName pkg) graph
            , pathEndsAt pkg path
            ]
       in case paths of
            [] ->
              Nothing

            _ ->
              Just
                FindingExplanation
                  { explainedPackage = pkg
                  , explainedRelationship = relationshipFromPaths paths
                  , explainedPaths = paths
                  }

findMentionedPackage :: PlanGraph -> Value -> PackageMatch
findMentionedPackage graph result =
  case candidates of
    [] -> NoPackageMention
    [pkg] -> MatchedPackage pkg
    pkgs@(pkg : _) -> AmbiguousPackageMention (packageName pkg) pkgs
 where
  mentionedNames =
    extractConcernedPackageNames $
      case result of
        Object resultObject -> maybe "" (Text.unwords . collectStrings) (KeyMap.lookup "message" resultObject)
        _ -> ""

  -- Several Cabal units can represent the same package version. Conversely,
  -- a version substring elsewhere in an advisory does not identify a unit.
  candidates =
    Map.elems $ Map.fromList
      [ ((packageName pkg, packageVersion pkg), pkg)
    | pkg <- Map.elems (planGraphPackages graph)
    , packageSource pkg == PackageExternal
    , packageName pkg `elem` mentionedNames
    ]

extractConcernedPackageNames :: Text -> [PackageName]
extractConcernedPackageNames haystack =
  map PackageName $
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

plausiblePackageName :: Text -> Bool
plausiblePackageName name =
  not (Text.null name)
    && Text.all validChar name
 where
  validChar c =
    isAsciiLower c
      || isAsciiUpper c
      || isDigit c
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
      [_localRoot, _target] -> True
      _                     -> False

addExplanation :: CabalLineIndex -> FilePath -> FindingExplanation -> KeyMap Value -> KeyMap Value
addExplanation lineIndex repoRoot explanation =
  addLocations lineIndex repoRoot explanation
    . addProperties explanation
    . addMessageExplanation explanation

addLocations :: CabalLineIndex -> FilePath -> FindingExplanation -> KeyMap Value -> KeyMap Value
addLocations lineIndex repoRoot explanation o =
  case explanationLocations lineIndex repoRoot explanation of
    []        -> o
    locations -> KeyMap.insert "locations" (Array (Vector.fromList locations)) o

explanationLocations :: CabalLineIndex -> FilePath -> FindingExplanation -> [Value]
explanationLocations lineIndex repoRoot explanation =
  [ locationValue cabalFile line
  | path <- explainedPaths explanation
  , (root, directDep) <- maybeToList (localRootAndDirectDependency path)
  , sourcePath <- maybeToList (packageSourcePath root)
  , let cabalFile = repoRelativeCabalFile repoRoot (localPackageCabalFile root sourcePath)
  , hasIndexedCabalFile lineIndex cabalFile
  , let line = lookupDependencyLine lineIndex cabalFile root directDep
  ]

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
      Just (String t) -> Just t
      _ -> Nothing

  message' =
    case oldMarkdown of
      Just markdown -> KeyMap.insert "markdown" (String (appendExplanationMarkdown markdown explanation)) messageWithText
      Nothing -> messageWithText

  messageWithText =
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
        ])
      oldProperties

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

sarifRunRoot :: KeyMap Value -> Maybe FilePath
sarifRunRoot run =
  case KeyMap.lookup "artifacts" run of
    Just (Array artifacts) ->
      asum
        [ uriToPath <$> artifactUri artifact
        | artifact <- Vector.toList artifacts
        ]

    _ ->
      Nothing

artifactUri :: Value -> Maybe Text
artifactUri =
  \case
    Object artifact ->
      case KeyMap.lookup "location" artifact of
        Just (Object location) ->
          case KeyMap.lookup "uri" location of
            Just (String uri) -> Just uri
            _ -> Nothing

        _ ->
          Nothing

    _ ->
      Nothing

guessRepoRoot :: PlanGraph -> FilePath
guessRepoRoot graph =
  case rootLocalPackages graph of
    [] ->
      "."

    [pkg] ->
      case packageSourcePath pkg of
        Just path ->
          dropTrailingPathSeparator (normalise path)

        Nothing ->
          "."

    pkgs ->
      commonParent
        [ path
        | pkg <- pkgs
        , path <- maybeToList (packageSourcePath pkg)
        ]

rootLocalPackages :: PlanGraph -> [Package]
rootLocalPackages graph =
  [ pkg
  | pkg <- Map.elems (planGraphPackages graph)
  , packageSource pkg == PackageLocal
  ]

commonParent :: [FilePath] -> FilePath
commonParent paths =
  case map (normalise . dropTrailingPathSeparator) paths of
    [] ->
      "."

    [path] ->
      path

    path : rest ->
      foldl commonPrefixPath path rest

commonPrefixPath :: FilePath -> FilePath -> FilePath
commonPrefixPath a b =
  joinPathParts $
    map fst $
      takeWhile (uncurry (==)) $
        zip (splitPathParts a) (splitPathParts b)

splitPathParts :: FilePath -> [FilePath]
splitPathParts =
  filter (not . null) . splitOnSlash . normalise

joinPathParts :: [FilePath] -> FilePath
joinPathParts parts =
  case parts of
    [] ->
      "."

    _ ->
      "/" <> foldr1 (\x y -> x <> "/" <> y) parts

splitOnSlash :: FilePath -> [FilePath]
splitOnSlash =
  go []
 where
  go acc [] =
    [reverse acc]

  go acc ('/' : xs) =
    reverse acc : go [] xs

  go acc (x : xs) =
    go (x : acc) xs

uriToPath :: Text -> FilePath
uriToPath uri =
  Text.unpack $
    stripFileUriPrefix uri

stripFileUriPrefix :: Text -> Text
stripFileUriPrefix uri
  | "file:////" `Text.isPrefixOf` uri =
      "/" <> Text.drop 9 uri

  | "file:///" `Text.isPrefixOf` uri =
      "/" <> Text.drop 8 uri

  | "file://" `Text.isPrefixOf` uri =
      Text.drop 7 uri

  | otherwise =
      uri


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
  , Version(..)
  )
import Hgs.DependencyUsage (Usage(..), packageUsages)
import Hgs.LocalUnitFilter
  ( LocalUnitFilter
  )
import Hgs.Paths
  ( PackagePath(..)
  , renderPackagePath
  )
import Hgs.Why (shortestPathsToPackageVersionFrom)
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
  , explainedUses         :: Set.Set Usage
  }
  deriving stock (Eq, Show)

data PackageMatch
  = MissingPackage PackageName
  | MatchedPackage FindingExplanation
  | AmbiguousPackageMention PackageName [Package]
  deriving stock (Eq, Show)

type UsageIndex = Map.Map (PackageName, Version) (Set.Set Usage)

enrichSarifValue :: CabalLineIndex -> LocalUnitFilter -> PlanGraph -> Value -> Value
enrichSarifValue lineIndex localFilter graph =
  \case
    Object o ->
      Object (adjustArrayField "runs" (enrichRun lineIndex localFilter graph usages) o)

    other ->
      other
 where
  usages = packageUsages graph

enrichRun :: CabalLineIndex -> LocalUnitFilter -> PlanGraph -> UsageIndex -> Value -> Value
enrichRun lineIndex localFilter graph usages =
  \case
    Object o ->
      let repoRoot =
            fromMaybe
              (guessRepoRoot graph)
              (sarifRunRoot o)
       in Object (adjustArrayField "results" (enrichResult lineIndex repoRoot localFilter graph usages) o)

    other ->
      other

enrichResult :: CabalLineIndex -> FilePath -> LocalUnitFilter -> PlanGraph -> UsageIndex -> Value -> Value
enrichResult lineIndex repoRoot localFilter graph usages result =
  case result of
    Object o ->
      let matches = findMentionedPackages localFilter graph usages result
          withMetadata = addMatchMetadata matches o
       in Object $ case matches of
            [MatchedPackage explanation] -> addExplanation lineIndex repoRoot explanation withMetadata
            [] -> withMetadata
            _ -> addMultipleExplanations lineIndex repoRoot matches withMetadata

    _ ->
      result

findMentionedPackages :: LocalUnitFilter -> PlanGraph -> UsageIndex -> Value -> [PackageMatch]
findMentionedPackages localFilter graph usages result = map match mentionedNames
 where
  mentionedNames =
    extractConcernedPackageNames $
      case result of
        Object resultObject -> case KeyMap.lookup "message" resultObject of
          Just (Object message) -> case KeyMap.lookup "text" message of
            Just (String messageText) -> messageText
            _ -> ""
          _ -> ""
        _ -> ""

  -- Several Cabal units can represent the same package version. Conversely,
  -- a version substring elsewhere in an advisory does not identify a unit.
  candidates name =
    Map.elems $ Map.fromList
      [ ((packageName pkg, packageVersion pkg), pkg)
    | pkg <- Map.elems (planGraphPackages graph)
    , packageSource pkg == PackageExternal
    , packageName pkg == name
    ]

  match name = case candidates name of
    [] -> MissingPackage name
    [pkg] -> MatchedPackage (explainPackage localFilter graph usages pkg)
    pkgs -> AmbiguousPackageMention name pkgs

explainPackage :: LocalUnitFilter -> PlanGraph -> UsageIndex -> Package -> FindingExplanation
explainPackage localFilter graph usages pkg =
  FindingExplanation
    { explainedPackage = pkg
    , explainedRelationship = relationshipFromPaths paths
    , explainedPaths = paths
    , explainedUses = Map.findWithDefault Set.empty (packageName pkg, packageVersion pkg) usages
    }
 where
  paths = shortestPathsToPackageVersionFrom localFilter (packageName pkg) (packageVersion pkg) graph

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

relationshipFromPaths :: [PackagePath] -> Text
relationshipFromPaths paths
  | null paths = "unknown"
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
    locations -> replaceLocations locations o

replaceLocations :: [Value] -> KeyMap Value -> KeyMap Value
replaceLocations locations o =
  KeyMap.insert "locations" (Array (Vector.fromList locations)) (saveOriginal o)
 where
  saveOriginal result = case KeyMap.lookup "locations" result of
    Nothing -> result
    Just original -> addPropertiesPreserving [("cabal-plan-submit.original-locations", original)] result

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
  oldText
    <> "\n\ncabal-plan-submit:\n"
    <> "  package: "
    <> renderPackageText (explainedPackage explanation)
    <> "\n"
    <> "  relationship: "
    <> explainedRelationship explanation
    <> "\n"
    <> "  usage: " <> renderUsages (explainedUses explanation) <> "\n"
    <> "  paths:\n"
    <> Text.concat
      [ "    - " <> Text.pack (renderPackagePath path) <> "\n"
      | path <- explainedPaths explanation
      ]

appendExplanationMarkdown :: Text -> FindingExplanation -> Text
appendExplanationMarkdown oldMarkdown explanation =
  oldMarkdown
    <> "\n\n## cabal-plan-submit dependency path\n\n"
    <> "* package: `"
    <> renderPackageText (explainedPackage explanation)
    <> "`\n"
    <> "* relationship: `"
    <> explainedRelationship explanation
    <> "`\n"
    <> "* usage: `" <> renderUsages (explainedUses explanation) <> "`\n"
    <> "* paths:\n"
    <> Text.concat
      [ "  * `"
          <> Text.pack (renderPackagePath path)
          <> "`\n"
      | path <- explainedPaths explanation
      ]

addProperties :: FindingExplanation -> KeyMap Value -> KeyMap Value
addProperties explanation o =
  addPropertiesPreserving
        [ ("cabal-plan-submit.package", String (renderPackageText (explainedPackage explanation)))
        , ("cabal-plan-submit.relationship", String (explainedRelationship explanation))
        , ("cabal-plan-submit.usage", Aeson.toJSON (map usageName (Set.toAscList (explainedUses explanation))))
        , ( "cabal-plan-submit.paths"
          , Aeson.toJSON
              [ Text.pack (renderPackagePath path)
              | path <- explainedPaths explanation
              ]
          )
        ] o

addPropertiesPreserving :: [(Key.Key, Value)] -> KeyMap Value -> KeyMap Value
addPropertiesPreserving additions o = case KeyMap.lookup "properties" o of
  Just (Object properties) -> insert properties
  Nothing -> insert KeyMap.empty
  Just _ -> o
 where
  insert properties = KeyMap.insert "properties" (Object (KeyMap.union properties (KeyMap.fromList additions))) o

addMatchMetadata :: [PackageMatch] -> KeyMap Value -> KeyMap Value
addMatchMetadata matches = addPropertiesPreserving $
  [ ("cabal-plan-submit.match", String status)
  , ("cabal-plan-submit.packages", Aeson.toJSON (map matchValue matches))
  ] <> case matches of
    [MatchedPackage _] -> []
    _ -> [("cabal-plan-submit.relationship", String "unknown")]
 where
  status = case matches of
    [] -> "unrecognized"
    [MissingPackage _] -> "not-in-plan"
    [AmbiguousPackageMention _ _] -> "ambiguous-version"
    [MatchedPackage _] -> "matched"
    _ -> "multiple-packages"

matchValue :: PackageMatch -> Value
matchValue (MissingPackage name) = Aeson.object
  ["name" Aeson..= unPackageName name, "match" Aeson..= ("not-in-plan" :: Text)]
matchValue (AmbiguousPackageMention name packages) = Aeson.object
  [ "name" Aeson..= unPackageName name
  , "match" Aeson..= ("ambiguous-version" :: Text)
  , "versions" Aeson..= map (unVersion . packageVersion) packages
  ]
matchValue (MatchedPackage explanation) = Aeson.object
  [ "name" Aeson..= unPackageName (packageName pkg)
  , "version" Aeson..= unVersion (packageVersion pkg)
  , "match" Aeson..= ("matched" :: Text)
  , "relationship" Aeson..= explainedRelationship explanation
  , "usage" Aeson..= map usageName (Set.toAscList (explainedUses explanation))
  , "paths" Aeson..= map (Text.pack . renderPackagePath) (explainedPaths explanation)
  ]
 where
  pkg = explainedPackage explanation

usageName :: Usage -> Text
usageName Production = "production-component"
usageName Test = "test"
usageName Benchmark = "benchmark"
usageName Build = "build"
usageName Unknown = "unknown"

renderUsages :: Set.Set Usage -> Text
renderUsages uses
  | Set.null uses = "unknown"
  | otherwise = Text.intercalate ", " (map usageName (Set.toAscList uses))

addMultipleExplanations :: CabalLineIndex -> FilePath -> [PackageMatch] -> KeyMap Value -> KeyMap Value
addMultipleExplanations lineIndex repoRoot matches = addMultiLocations . addMultiMessage
 where
  explanations = [explanation | MatchedPackage explanation <- matches]
  locations = concatMap (explanationLocations lineIndex repoRoot) explanations
  addMultiLocations result = if null locations then result else replaceLocations locations result
  addMultiMessage result = case KeyMap.lookup "message" result of
    Just (Object message) -> KeyMap.insert "message" (Object (appendMessages message)) result
    _ -> result
  appendMessages message =
    let summary = Text.concat (map renderMatch matches)
        add field prefix object = case KeyMap.lookup field object of
          Just existing -> KeyMap.insert field (appendText (prefix <> summary) existing) object
          Nothing -> object
     in add "markdown" "\n\n## cabal-plan-submit package matches\n\n" $
          add "text" "\n\ncabal-plan-submit package matches:\n" message
  appendText suffix (String original) = String (original <> suffix)
  appendText _ other = other

renderMatch :: PackageMatch -> Text
renderMatch (MissingPackage name) = "- " <> unPackageName name <> ": not in plan\n"
renderMatch (AmbiguousPackageMention name packages) =
  "- " <> unPackageName name <> ": ambiguous versions "
    <> Text.intercalate ", " (map (unVersion . packageVersion) packages) <> "\n"
renderMatch (MatchedPackage explanation) =
  "- " <> renderPackageText (explainedPackage explanation)
    <> ": " <> explainedRelationship explanation <> "; " <> renderUsages (explainedUses explanation) <> "\n"
    <> Text.concat ["  - " <> Text.pack (renderPackagePath path) <> "\n" | path <- explainedPaths explanation]

adjustArrayField :: Text -> (Value -> Value) -> KeyMap Value -> KeyMap Value
adjustArrayField field f o =
  case KeyMap.lookup (Key.fromText field) o of
    Just (Array xs) ->
      KeyMap.insert (Key.fromText field) (Array (Vector.map f xs)) o

    _ ->
      o

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


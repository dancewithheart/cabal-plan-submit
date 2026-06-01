{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hgs.Sarif.Deprecated
  ( deprecatedPackagesSarif
  ) where

import Data.Aeson (Value(..))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Hgs.Deprecated
  ( DeprecatedPackage
  , deprecatedPackageName
  , deprecatedPackageVersion
  , deprecatedPath
  , deprecatedRelationship
  , deprecatedReplacements
  )
import Hgs.Domain
  ( Package(..)
  , PackageName(..)
  , Version(..)
  )
import Hgs.Sarif.Enrich
  ( CabalLineIndex
  )
import Hgs.Why
  ( PackagePath(..)
  , renderPackagePath
  )
import System.FilePath
  ( (</>)
  , makeRelative
  , normalise
  , takeExtension
  )

deprecatedPackagesSarif :: CabalLineIndex -> FilePath -> [DeprecatedPackage] -> Value
deprecatedPackagesSarif lineIndex repoRoot deprecated =
  Object $
    KeyMap.fromList
      [ ("version", String "2.1.0")
      , ( "$schema"
        , String "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/master/Schemata/sarif-schema-2.1.0.json"
        )
      , ( "runs"
        , Array $
            Vector.singleton $
              Object $
                KeyMap.fromList
                  [ ("tool", tool deprecated)
                  , ("results", Array (Vector.fromList (map (deprecatedResult lineIndex repoRoot) deprecated)))
                  ]
        )
      ]

tool :: [DeprecatedPackage] -> Value
tool deprecated =
  Object $
    KeyMap.fromList
      [
        ( "driver"
        , Object $
            KeyMap.fromList
              [ ("name", String "cabal-plan-submit")
              , ("informationUri", String "https://github.com/dancewithheart/cabal-plan-submit")
              , ("rules", Array $ Vector.fromList (map deprecatedRule deprecated))
              ]
        )
      ]

deprecatedRule :: DeprecatedPackage -> Value
deprecatedRule dep =
  Object $
    KeyMap.fromList
      [ ("id", String (deprecatedRuleId dep))
      , ("name", String (deprecatedRuleTitle dep))
      ,
        ( "shortDescription"
        , Object $
            KeyMap.fromList
              [ ("text", String (deprecatedRuleTitle dep))
              ]
        )
      ,
        ( "fullDescription"
        , Object $
            KeyMap.fromList
              [
                ( "text"
                , String $
                    renderDeprecatedPackage dep
                      <> " is a resolved Cabal dependency marked as deprecated in Hackage metadata."
                )
              ]
        )
      ,
        ( "properties"
        , Object $
            KeyMap.fromList
              [ ( "tags"
                , Aeson.toJSON
                    [ "haskell" :: Text
                    , "cabal"
                    , "hackage"
                    , "deprecated-dependency"
                    , unPackageName (deprecatedPackageName dep)
                    ]
                )
              , ("precision", String "medium")
              , ("problem.severity", String (deprecatedProblemSeverity dep))
              ]
        )
      ]

deprecatedRuleId :: DeprecatedPackage -> Text
deprecatedRuleId dep =
  "haskell.deprecated-package."
    <> unPackageName (deprecatedPackageName dep)
    <> "."
    <> unVersion (deprecatedPackageVersion dep)

deprecatedRuleTitle :: DeprecatedPackage -> Text
deprecatedRuleTitle dep =
  "Deprecated Hackage package: " <> renderDeprecatedPackage dep

deprecatedResult :: CabalLineIndex -> FilePath -> DeprecatedPackage -> Value
deprecatedResult lineIndex repoRoot dep =
  Object $
    KeyMap.fromList
      [ ("ruleId", String (deprecatedRuleId dep))
      , ("level", String (deprecatedLevel dep))
      , ("message", deprecatedMessage dep)
      , ("locations", Array (Vector.fromList (deprecatedLocations lineIndex repoRoot dep)))
      , ("properties", deprecatedProperties dep)
      ]

deprecatedLevel :: DeprecatedPackage -> Text
deprecatedLevel dep =
  case deprecatedRelationship dep of
    "direct" ->
      "warning"

    _ ->
      "note"

deprecatedMessage :: DeprecatedPackage -> Value
deprecatedMessage dep =
  Object $
    KeyMap.fromList
      [ ("text", String (deprecatedText dep))
      , ("markdown", String (deprecatedMarkdown dep))
      ]

deprecatedText :: DeprecatedPackage -> Text
deprecatedText dep =
  renderDeprecatedPackage dep
    <> " is deprecated.\n"
    <> renderReplacementText dep
    <> renderPathText dep

deprecatedMarkdown :: DeprecatedPackage -> Text
deprecatedMarkdown dep =
  "## Deprecated Hackage package\n\n"
    <> "* package: `"
    <> renderDeprecatedPackage dep
    <> "`\n"
    <> "* relationship: `"
    <> deprecatedRelationship dep
    <> "`\n"
    <> renderReplacementMarkdown dep
    <> renderPathMarkdown dep

renderDeprecatedPackage :: DeprecatedPackage -> Text
renderDeprecatedPackage dep =
  unPackageName (deprecatedPackageName dep)
    <> "-"
    <> unVersion (deprecatedPackageVersion dep)

renderReplacementText :: DeprecatedPackage -> Text
renderReplacementText dep =
  case deprecatedReplacements dep of
    [] ->
      "No replacement package listed.\n"

    replacements ->
      "Replacements: "
        <> Text.intercalate ", " (map unPackageName replacements)
        <> "\n"

renderReplacementMarkdown :: DeprecatedPackage -> Text
renderReplacementMarkdown dep =
  case deprecatedReplacements dep of
    [] ->
      "* replacements: none listed\n"

    replacements ->
      "* replacements:\n"
        <> Text.concat
          [ "  * `"
              <> unPackageName replacement
              <> "`\n"
          | replacement <- replacements
          ]

renderPathText :: DeprecatedPackage -> Text
renderPathText dep =
  case deprecatedPath dep of
    Nothing ->
      ""

    Just path ->
      "Path: " <> Text.pack (renderPackagePath path) <> "\n"

renderPathMarkdown :: DeprecatedPackage -> Text
renderPathMarkdown dep =
  case deprecatedPath dep of
    Nothing ->
      ""

    Just path ->
      "* path: `"
        <> Text.pack (renderPackagePath path)
        <> "`\n"

deprecatedProperties :: DeprecatedPackage -> Value
deprecatedProperties dep =
  Object $
    KeyMap.fromList
      [ ("cabal-plan-submit.package", String (renderDeprecatedPackage dep))
      , ("cabal-plan-submit.relationship", String (deprecatedRelationship dep))
      , ("cabal-plan-submit.replacements", Aeson.toJSON (map unPackageName (deprecatedReplacements dep)))
      , ("cabal-plan-submit.paths", Aeson.toJSON (maybeToList (Text.pack . renderPackagePath <$> deprecatedPath dep)))
      , ("precision", String "medium")
      , ("problem.severity", String (deprecatedProblemSeverity dep))
      , ("tags", Aeson.toJSON (deprecatedTags dep))
      ]

deprecatedProblemSeverity :: DeprecatedPackage -> Text
deprecatedProblemSeverity dep =
  case deprecatedRelationship dep of
    "direct" ->
      "warning"

    _ ->
      "recommendation"

deprecatedTags :: DeprecatedPackage -> [Text]
deprecatedTags dep =
  [ "haskell"
  , "cabal"
  , "hackage"
  , "deprecated-dependency"
  , case deprecatedRelationship dep of
      "direct" -> "direct-dependency"
      _        -> "transitive-dependency"
  ]

deprecatedLocations :: CabalLineIndex -> FilePath -> DeprecatedPackage -> [Value]
deprecatedLocations lineIndex repoRoot dep =
  [ locationValue cabalFile line
  | path <- maybeToList (deprecatedPath dep)
  , (root, directDep) <- maybeToList (localRootAndDirectDependency path)
  , sourcePath <- maybeToList (packageSourcePath root)
  , let cabalFile = repoRelativePath repoRoot (localPackageCabalFile root sourcePath)
  , let line = Map.lookup (cabalFile, packageName directDep) lineIndex
  ]

localRootAndDirectDependency :: PackagePath -> Maybe (Package, Package)
localRootAndDirectDependency path =
  case unPackagePath path of
    root : directDep : _ ->
      Just (root, directDep)

    _ ->
      Nothing

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

localPackageCabalFile :: Package -> FilePath -> FilePath
localPackageCabalFile pkg path
  | takeExtension path == ".cabal" =
      normalise path

  | otherwise =
      normalise path </> Text.unpack (unPackageName (packageName pkg)) <> ".cabal"

repoRelativePath :: FilePath -> FilePath -> FilePath
repoRelativePath repoRoot path =
  normalise $
    makeRelative
      (normalise repoRoot)
      (normalise path)

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hgs.Validate
  ( ValidationErrors(..)
  , validateSnapshotFile
  , validateSnapshotValue
  , renderValidationReport
  , isValid
  ) where

import Data.Aeson (Value(..), eitherDecodeStrict')
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.List (nub)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector

newtype ValidationErrors = ValidationErrors
  { getValidationErrors :: [Text]
  }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

validateSnapshotFile :: FilePath -> IO (Either String ValidationErrors)
validateSnapshotFile path =
  fmap (fmap validateSnapshotValue . eitherDecodeStrict') (BS.readFile path)

validateSnapshotValue :: Value -> ValidationErrors
validateSnapshotValue =
  \case
    Object o ->
      topLevelChecks o <> manifestsChecks o
    _ ->
      oneError "$: expected top-level JSON object"

isValid :: ValidationErrors -> Bool
isValid = null . getValidationErrors

renderValidationReport :: ValidationErrors -> String
renderValidationReport errs
  | isValid errs = "snapshot validation OK"
  | otherwise =
      unlines $
        "snapshot validation FAILED:"
          : map (("  - " <>) . Text.unpack) (getValidationErrors errs)

topLevelChecks :: KeyMap.KeyMap Value -> ValidationErrors
topLevelChecks o =
  foldMap (requireKey o)
    [ "version"
    , "sha"
    , "ref"
    , "job"
    , "detector"
    , "manifests"
    , "scanned"
    ]

requireKey :: KeyMap.KeyMap Value -> Text -> ValidationErrors
requireKey o key
  | KeyMap.member (Key.fromText key) o = mempty
  | otherwise = oneError ("$: missing required key " <> key)

manifestsChecks :: KeyMap.KeyMap Value -> ValidationErrors
manifestsChecks o =
  case KeyMap.lookup "manifests" o of
    Nothing ->
      oneError "$.manifests: missing"
    Just (Object manifests)
      | KeyMap.null manifests ->
          oneError "$.manifests: expected one or more manifests"
      | otherwise ->
          foldMap validateManifest (KeyMap.toList manifests)
    Just _ ->
      oneError "$.manifests: expected object"

validateManifest :: (Key.Key, Value) -> ValidationErrors
validateManifest (manifestKey, manifestValue) =
  case manifestValue of
    Object o ->
      requireManifestName o
        <> requireManifestResolved o
        <> validateResolvedObject (keyPath ["manifests", Key.toText manifestKey, "resolved"]) (lookupObject "resolved" o)
    _ ->
      oneError (keyPath ["manifests", Key.toText manifestKey] <> ": expected object")

requireManifestName :: KeyMap.KeyMap Value -> ValidationErrors
requireManifestName o =
  case KeyMap.lookup "name" o of
    Just (String _) -> mempty
    Just _ -> oneError "$.manifests.*.name: expected string"
    Nothing -> oneError "$.manifests.*.name: missing"

requireManifestResolved :: KeyMap.KeyMap Value -> ValidationErrors
requireManifestResolved o =
  case KeyMap.lookup "resolved" o of
    Just (Object _) -> mempty
    Just _ -> oneError "$.manifests.*.resolved: expected object"
    Nothing -> oneError "$.manifests.*.resolved: missing"

validateResolvedObject :: Text -> Maybe (KeyMap.KeyMap Value) -> ValidationErrors
validateResolvedObject path =
  \case
    Nothing ->
      mempty
    Just resolved ->
      let resolvedKeys =
            map Key.toText (KeyMap.keys resolved)
       in foldMap (validateResolvedEntry path resolvedKeys) (KeyMap.toList resolved)

validateResolvedEntry :: Text -> [Text] -> (Key.Key, Value) -> ValidationErrors
validateResolvedEntry basePath resolvedKeys (resolvedKey, entryValue) =
  case entryValue of
    Object o ->
      let here = keyPath [basePath, Key.toText resolvedKey]
          (pkgUrlErrs, mPkgUrl) = expectStringField here "package_url" o
          (depsErrs, deps) = expectStringArrayField here "dependencies" o
          selfEdgeErrs =
            case mPkgUrl of
              Just pkgUrl
                | pkgUrl `elem` deps ->
                    oneError (here <> ".dependencies: contains self package_url " <> pkgUrl)
              _ ->
                mempty
          closureErrs =
            foldMap
              (\dep ->
                 if dep `elem` resolvedKeys
                   then mempty
                   else oneError (here <> ".dependencies: unknown target " <> dep))
              deps
          duplicateErrs =
            if hasDuplicates deps
              then oneError (here <> ".dependencies: duplicate entries")
              else mempty
       in pkgUrlErrs <> depsErrs <> selfEdgeErrs <> closureErrs <> duplicateErrs
    _ ->
      oneError (keyPath [basePath, Key.toText resolvedKey] <> ": expected object")

expectStringField :: Text -> Text -> KeyMap.KeyMap Value -> (ValidationErrors, Maybe Text)
expectStringField here field o =
  case KeyMap.lookup (Key.fromText field) o of
    Just (String t) -> (mempty, Just t)
    Just _ -> (oneError (here <> "." <> field <> ": expected string"), Nothing)
    Nothing -> (oneError (here <> "." <> field <> ": missing"), Nothing)

expectStringArrayField :: Text -> Text -> KeyMap.KeyMap Value -> (ValidationErrors, [Text])
expectStringArrayField here field o =
  case KeyMap.lookup (Key.fromText field) o of
    Just (Array arr) ->
      let vals = Vector.toList arr
          bad =
            [ oneError (here <> "." <> field <> ": expected array of strings")
            | any (not . isString) vals
            ]
       in (mconcat bad, [t | String t <- vals])
    Just _ ->
      (oneError (here <> "." <> field <> ": expected array"), [])
    Nothing ->
      (oneError (here <> "." <> field <> ": missing"), [])

lookupObject :: Text -> KeyMap.KeyMap Value -> Maybe (KeyMap.KeyMap Value)
lookupObject field o =
  case KeyMap.lookup (Key.fromText field) o of
    Just (Object x) -> Just x
    _ -> Nothing

isString :: Value -> Bool
isString =
  \case
    String _ -> True
    _ -> False

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates xs =
  length xs /= length (nub xs)

keyPath :: [Text] -> Text
keyPath =
  Text.intercalate "." . ("$" :)

oneError :: Text -> ValidationErrors
oneError e =
  ValidationErrors [e]

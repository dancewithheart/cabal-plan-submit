{-# LANGUAGE LambdaCase #-}

module Hgs.CabalFileIndex
  ( CabalLineIndex
  , buildCabalLineIndex
  , indexCabalFile
  , localPackageCabalFile
  , repoRelativeCabalFile
  , guessRepoRoot
  ) where

import Data.Char (isAlphaNum)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Text qualified as Text
import System.Directory (doesFileExist)
import System.FilePath
  ( (</>)
  , dropTrailingPathSeparator
  , joinPath
  , makeRelative
  , normalise
  , splitDirectories
  , takeExtension
  )

import Hgs.Domain
  ( Package(..)
  , PackageName(..)
  , PackageSource(..)
  , PlanGraph(..)
  )

type CabalLineIndex = Map (FilePath, PackageName) Int

buildCabalLineIndex :: FilePath -> PlanGraph -> IO CabalLineIndex
buildCabalLineIndex repoRoot graph =
  Map.unions <$>
    traverse (cabalLineIndexForLocalPackage repoRoot) localPackages
 where
  localPackages =
    [ pkg
    | pkg <- Map.elems (planGraphPackages graph)
    , packageSource pkg == PackageLocal
    ]

cabalLineIndexForLocalPackage :: FilePath -> Package -> IO CabalLineIndex
cabalLineIndexForLocalPackage repoRoot pkg =
  case packageSourcePath pkg of
    Nothing ->
      pure Map.empty

    Just sourcePath -> do
      let cabalFile = localPackageCabalFile pkg sourcePath
      exists <- doesFileExist cabalFile
      if exists
        then do
          contents <- readFile cabalFile
          pure (indexCabalFile repoRoot cabalFile contents)
        else
          pure Map.empty

indexCabalFile :: FilePath -> FilePath -> String -> CabalLineIndex
indexCabalFile repoRoot cabalFile contents =
  Map.fromListWith min
    [ ( (repoRelativeCabalFile repoRoot cabalFile, PackageName (Text.pack token))
      , lineNo
      )
    | (lineNo, line) <- zip [1 :: Int ..] (lines contents)
    , token <- packageTokens line
    ]

packageTokens :: String -> [String]
packageTokens =
  filter plausiblePackageName
    . wordsBy (not . isPackageNameChar)
    . stripLineComment

stripLineComment :: String -> String
stripLineComment =
  takeUntilDashDash
 where
  takeUntilDashDash [] =
    []

  takeUntilDashDash [x] =
    [x]

  takeUntilDashDash ('-' : '-' : _) =
    []

  takeUntilDashDash (x : xs) =
    x : takeUntilDashDash xs

isPackageNameChar :: Char -> Bool
isPackageNameChar c = isAlphaNum c || c == '-' || c == '_'

plausiblePackageName :: String -> Bool
plausiblePackageName token = '-' `elem` token || all isAlphaNum token

wordsBy :: (Char -> Bool) -> String -> [String]
wordsBy p = go
 where
  go [] = []
  go xs =
    case dropWhile p xs of
      [] -> []
      ys ->
        let (word, rest) = break p ys
         in word : go rest

localPackageCabalFile :: Package -> FilePath -> FilePath
localPackageCabalFile pkg path
  | takeExtension path == ".cabal" =
      normalise path
  | otherwise =
      normalise path </> Text.unpack (unPackageName (packageName pkg)) <> ".cabal"

repoRelativeCabalFile :: FilePath -> FilePath -> FilePath
repoRelativeCabalFile repoRoot path =
  normalise $
    makeRelative
      (normalise repoRoot)
      (normalise path)

guessRepoRoot :: PlanGraph -> FilePath
guessRepoRoot graph =
  case localSourcePaths of
    [] -> "."
    [path] -> dropTrailingPathSeparator (normalise path)
    path : rest -> foldl commonPrefixPath path rest
 where
  localSourcePaths =
    [ dropTrailingPathSeparator (normalise path)
    | pkg <- Map.elems (planGraphPackages graph)
    , packageSource pkg == PackageLocal
    , path <- maybeToList (packageSourcePath pkg)
    ]

commonPrefixPath :: FilePath -> FilePath -> FilePath
commonPrefixPath a b =
  joinPathParts $
    map fst $
      takeWhile (uncurry (==)) $
        zip (splitPathParts a) (splitPathParts b)

splitPathParts :: FilePath -> [FilePath]
splitPathParts = splitDirectories . normalise

joinPathParts :: [FilePath] -> FilePath
joinPathParts = \case
  [] -> "."
  xs -> joinPath xs

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Hgs.PathTrie
  ( PathTrie(..)
  , pathTrieFromPaths
  , renderPathTrie
  ) where

import Data.Maybe (maybeToList)
import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Hgs.Domain
  ( Package(..)
  , PackageName(..)
  , Version(..)
  )
import Hgs.Paths
  ( PackageKey
  , PackagePath(..)
  , packageKey
  )

data PathTrie = PathTrie
  { triePackage  :: Package
  , trieChildren :: Map PackageKey PathTrie
  }
  deriving stock (Eq, Show)

pathTrieFromPaths :: [PackagePath] -> Maybe PathTrie
pathTrieFromPaths paths =
  case map reversePath paths of
    [] ->
      Nothing

    [] : _ ->
      Nothing

    (root : firstRest) : rest ->
      Just $
        foldl'
          insertReversedPath
          (insertReversedPath (singleton root) firstRest)
          [ pathRest
          | reversedPath <- rest
          , pathRest <- maybeToList (stripCommonRoot root reversedPath)
          ]
 where
  reversePath =
    reverse . unPackagePath

stripCommonRoot :: Package -> [Package] -> Maybe [Package]
stripCommonRoot root =
  \case
    [] ->
      Nothing

    x : xs
      | packageKey x == packageKey root ->
          Just xs
      | otherwise ->
          Nothing

insertReversedPath :: PathTrie -> [Package] -> PathTrie
insertReversedPath trie =
  \case
    [] ->
      trie

    pkg : rest ->
      trie
        { trieChildren =
            Map.alter
              (Just . insertChild pkg rest)
              (packageKey pkg)
              (trieChildren trie)
        }

insertChild :: Package -> [Package] -> Maybe PathTrie -> PathTrie
insertChild pkg rest =
  \case
    Nothing ->
      insertReversedPath (singleton pkg) rest

    Just existing ->
      insertReversedPath existing rest

singleton :: Package -> PathTrie
singleton pkg =
  PathTrie
    { triePackage = pkg
    , trieChildren = Map.empty
    }

renderPathTrie :: PathTrie -> String
renderPathTrie trie =
  unlines (renderNode [] True trie)

renderNode :: [Bool] -> Bool -> PathTrie -> [String]
renderNode ancestorsLast isLast trie =
  renderCurrent : renderChildren
 where
  renderCurrent =
    prefix ancestorsLast
      <> branch isLast
      <> renderPackage (triePackage trie)

  children =
    Map.elems (trieChildren trie)

  renderChildren =
    concat
      [ renderNode (ancestorsLast <> [isLast]) childIsLast child
      | (childIsLast, child) <- markLast children
      ]

prefix :: [Bool] -> String
prefix =
  concatMap
    (\ancestorIsLast -> if ancestorIsLast then "    " else "│   ")

branch :: Bool -> String
branch isLast =
  if isLast then "└── " else "├── "

markLast :: [a] -> [(Bool, a)]
markLast =
  \case
    [] ->
      []

    [x] ->
      [(True, x)]

    x : xs ->
      (False, x) : markLast xs

renderPackage :: Package -> String
renderPackage pkg =
  Text.unpack (unPackageName (packageName pkg))
    <> "-"
    <> Text.unpack (unVersion (packageVersion pkg))

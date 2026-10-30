{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hgs.CabalFileIndex
  ( CabalLineIndex(..)
  , DependencyDeclaration(..)
  , buildCabalLineIndex
  , indexCabalFile
  , lookupDependencyLine
  , hasIndexedCabalFile
  , localPackageCabalFile
  , repoRelativeCabalFile
  , guessRepoRoot
  ) where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.ByteString.Char8 qualified as BS8
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Vector qualified as Vector
import Distribution.Compat.CharParsing qualified as CabalChar
import Distribution.Fields.Field (fieldLinesToString)
import Distribution.Fields.Parser (Field(..), FieldLine(..), Name(..), SectionArg(..), readFields)
import Distribution.Parsec qualified as Cabal
import Distribution.Parsec.FieldLineStream (fieldLineStreamFromString)
import Distribution.Parsec.Position (Position, positionRow)
import Distribution.Types.Dependency qualified as Dependency
import Distribution.Types.ExeDependency qualified as ExeDependency
import Distribution.Types.PackageName qualified as CabalPackage
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
import System.IO (hPutStrLn, stderr)
import Text.Parsec qualified as Parsec

import Hgs.Domain
  ( ComponentName(..)
  , DependencyKind(..)
  , DependencyOrigin(..)
  , Package(..)
  , PackageName(..)
  , PackageSource(..)
  , PlanGraph(..)
  )

data DependencyDeclaration = DependencyDeclaration
  { declarationComponent :: ComponentName
  , declarationKind :: DependencyKind
  , declarationLine :: Int
  }
  deriving (Eq, Ord, Show)

data CabalLineIndex = CabalLineIndex
  { indexedCabalFiles :: Set.Set FilePath
  , dependencyDeclarations :: Map (FilePath, PackageName) (Set.Set DependencyDeclaration)
  }
  deriving (Eq, Show)

instance Semigroup CabalLineIndex where
  CabalLineIndex filesA declarationsA <> CabalLineIndex filesB declarationsB =
    CabalLineIndex (Set.union filesA filesB) (Map.unionWith Set.union declarationsA declarationsB)

instance Monoid CabalLineIndex where
  mempty = CabalLineIndex Set.empty Map.empty

buildCabalLineIndex :: FilePath -> PlanGraph -> IO CabalLineIndex
buildCabalLineIndex repoRoot graph =
  mconcat <$>
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
      pure mempty

    Just sourcePath -> do
      let cabalFile = localPackageCabalFile pkg sourcePath
      exists <- doesFileExist cabalFile
      if exists
        then do
          contents <- readFile cabalFile
          case indexCabalFile repoRoot cabalFile contents of
            Left err -> do
              hPutStrLn stderr ("Cannot index " <> cabalFile <> ": " <> err)
              pure (fileOnlyIndex repoRoot cabalFile)
            Right index ->
              pure index
        else
          pure mempty

fileOnlyIndex :: FilePath -> FilePath -> CabalLineIndex
fileOnlyIndex repoRoot cabalFile =
  CabalLineIndex (Set.singleton (repoRelativeCabalFile repoRoot cabalFile)) Map.empty

-- The result reports malformed syntax instead of guessing a location.
indexCabalFile :: FilePath -> FilePath -> String -> Either String CabalLineIndex
indexCabalFile repoRoot cabalFile contents = do
  fields <- first show (readFields (Text.encodeUtf8 (Text.pack contents)))
  commons <- commonStanzas fields
  locations <-
    if all isField fields
      then declarationsFor (ComponentName "lib") <$> componentLocations commons fields
      else concat <$> traverse (sectionLocations commons) fields
  pure $ CabalLineIndex
    (Set.singleton relativePath)
    (Map.fromListWith Set.union
      [ ((relativePath, name), Set.singleton declaration) | (name, declaration) <- locations ])
 where
  relativePath = repoRelativeCabalFile repoRoot cabalFile

type CommonStanzas = Map Text (Position, [Field Position])
type DependencyLocation = (PackageName, DependencyKind, Int)
type CommonImport = (Position, Text)

lookupDependencyLine :: CabalLineIndex -> FilePath -> Package -> Package -> Maybe Int
lookupDependencyLine index cabalFile root directDep = do
  declarations <- Map.lookup (cabalFile, packageName directDep) (dependencyDeclarations index)
  origins <- Map.lookup (packageUnitId directDep) (packageDependencies root)
  Set.lookupMin $ Set.fromList
    [ declarationLine declaration
    | declaration <- Set.toList declarations
    , origin <- Set.toList origins
    , dependencyComponent origin == Just (declarationComponent declaration)
    , dependencyKind origin == declarationKind declaration
    ]

hasIndexedCabalFile :: CabalLineIndex -> FilePath -> Bool
hasIndexedCabalFile index cabalFile = cabalFile `Set.member` indexedCabalFiles index

commonStanzas :: [Field Position] -> Either String CommonStanzas
commonStanzas = foldM addCommon Map.empty
 where
  addCommon commons (Section (Name pos "common") args fields) = do
    name <- case args of
      [SecArgName _ value] -> Right (Text.decodeUtf8 value)
      [SecArgStr _ value]  -> Right (Text.decodeUtf8 value)
      _ -> Left ("Invalid common stanza name at line " <> show (positionRow pos))
    if Map.member name commons
      then Left ("Duplicate common stanza: " <> Text.unpack name)
      else Right (Map.insert name (pos, fields) commons)
  addCommon commons _ = Right commons

isField :: Field ann -> Bool
isField (Field _ _) = True
isField _ = False

sectionLocations :: CommonStanzas -> Field Position -> Either String [(PackageName, DependencyDeclaration)]
sectionLocations commons = \case
  Section (Name _ name) args fields
    | name == "custom-setup" ->
        declarationsFor (ComponentName "setup") . concat <$> traverse setupLocations fields
    | Just component <- sectionComponent name args ->
        declarationsFor component <$> componentLocations commons fields
  _ -> Right []

declarationsFor :: ComponentName -> [DependencyLocation] -> [(PackageName, DependencyDeclaration)]
declarationsFor component = map (\(name, kind, line) -> (name, DependencyDeclaration component kind line))

sectionComponent :: BS8.ByteString -> [SectionArg Position] -> Maybe ComponentName
sectionComponent "library" [] = Just (ComponentName "lib")
sectionComponent name [arg] = do
  prefix <- case name of
    "library" -> Just "lib:"
    "foreign-library" -> Just "flib:"
    "executable" -> Just "exe:"
    "test-suite" -> Just "test:"
    "benchmark" -> Just "bench:"
    _ -> Nothing
  component <- case arg of
    SecArgName _ value -> Just (Text.decodeUtf8 value)
    SecArgStr _ value -> Just (Text.decodeUtf8 value)
    _ -> Nothing
  pure (ComponentName (prefix <> component))
sectionComponent _ _ = Nothing

-- Cabal accepts imports at the beginning of a component, common stanza, or
-- conditional branch. Unimported common stanzas are not dependencies.
componentLocations :: CommonStanzas -> [Field Position] -> Either String [DependencyLocation]
componentLocations commons fields = do
  (locations, imports) <- componentFields fields
  expandImports Set.empty locations imports
 where
  -- The minimum operation is idempotent, so each common stanza needs to be
  -- visited only once even when imports form a diamond or contain repeats.
  expandImports _ locations [] = Right locations
  expandImports seen locations ((pos, name) : imports) =
    case Map.lookup name commons of
      Just (definitionPos, commonFields) | definitionPos < pos ->
        if Set.member name seen
          then expandImports seen locations imports
          else do
            (newLocations, newImports) <- componentFields commonFields
            expandImports (Set.insert name seen) (newLocations <> locations) (newImports <> imports)
      _ -> Left ("Undefined common stanza at line " <> show (positionRow pos) <> ": " <> Text.unpack name)

componentFields :: [Field Position] -> Either String ([DependencyLocation], [CommonImport])
componentFields fields = do
  let (imports, body) = span isImport fields
  imported <- concat <$> traverse commonImports imports
  local <- traverse componentFieldLocations body
  pure (concatMap fst local, imported <> concatMap snd local)
 where
  isImport (Field (Name _ "import") _) = True
  isImport _ = False

commonImports :: Field Position -> Either String [CommonImport]
commonImports (Field fieldName@(Name pos _) fieldLines) = do
  names <- parseField fieldName fieldLines (Cabal.parsecLeadingCommaList Cabal.parsecToken)
  pure [(pos, Text.pack name) | name <- names]
commonImports _ = Right []

componentFieldLocations :: Field Position -> Either String ([DependencyLocation], [CommonImport])
componentFieldLocations = \case
  Field name@(Name _ "build-depends") fieldLines ->
    (,[]) . map (withKind ComponentDependency) <$> dependencyLocations name fieldLines (Cabal.parsec :: Cabal.ParsecParser Dependency.Dependency) Dependency.depPkgName
  Field name@(Name _ "build-tool-depends") fieldLines ->
    (,[]) . map (withKind ExecutableDependency) <$> dependencyLocations name fieldLines (Cabal.parsec :: Cabal.ParsecParser ExeDependency.ExeDependency) exePackage
  -- A plan edge alone does not say which condition made a declaration active.
  -- Until the condition is evaluated against the plan, omit these candidates.
  _ -> Right ([], [])
 where
  exePackage (ExeDependency.ExeDependency package _ _) = package

withKind :: DependencyKind -> (PackageName, Int) -> DependencyLocation
withKind kind (name, line) = (name, kind, line)

setupLocations :: Field Position -> Either String [DependencyLocation]
setupLocations (Field name@(Name _ "setup-depends") fieldLines) =
  map (withKind ComponentDependency) <$> dependencyLocations name fieldLines (Cabal.parsec :: Cabal.ParsecParser Dependency.Dependency) Dependency.depPkgName
setupLocations _ = Right []

dependencyLocations
  :: Name Position
  -> [FieldLine Position]
  -> Cabal.ParsecParser a
  -> (a -> CabalPackage.PackageName)
  -> Either String [(PackageName, Int)]
dependencyLocations name fieldLines dependencyParser packageName = do
  dependencies <- parseField name fieldLines (Cabal.parsecLeadingCommaList locatedDependency)
  traverse sourceLocation dependencies
 where
  locatedDependency = do
    virtualLine <- Parsec.sourceLine <$> Cabal.PP (const Parsec.getPosition)
    dependency <- dependencyParser
    pure (virtualLine, dependency)

  -- Cabal drops blank lines and comments from field contents. Translate the
  -- parser's line in that compact stream back to the original FieldLine.
  sourceLines = Vector.fromList [positionRow position | FieldLine position _ <- fieldLines]

  sourceLocation (virtualLine, dependency) =
    case sourceLines Vector.!? (virtualLine - 1) of
      Just line | line > 0 ->
        Right (PackageName (Text.pack (CabalPackage.unPackageName (packageName dependency))), line)
      _ -> Left "Cannot map dependency to a source line"

parseField :: Name Position -> [FieldLine Position] -> Cabal.ParsecParser a -> Either String a
parseField (Name position name) fieldLines parser =
  first describeError $
    Cabal.runParsecParser
      (CabalChar.spaces *> parser <* CabalChar.spaces)
      "<cabal field>"
      (fieldLineStreamFromString (fieldLinesToString fieldLines))
 where
  describeError err =
    "Invalid " <> BS8.unpack name <> " field at line " <> show (positionRow position) <> ": " <> show err

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

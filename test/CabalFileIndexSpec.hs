{-# LANGUAGE OverloadedStrings #-}

module CabalFileIndexSpec (spec) where

import Data.Aeson qualified as Aeson
import Data.Either (isLeft)
import Data.List (elemIndex)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Hgs.CabalFileIndex (buildCabalLineIndex, indexCabalFile)
import Hgs.Domain (PackageName(..), RawPkgSrc(..), RawPlan(..), RawPlanItem(..), UnitId(..))
import Hgs.Extract (extractPlanGraph)
import Hgs.LocalUnitFilter (LocalUnitFilter(..))
import Hgs.Sarif.Enrich (enrichSarifValue)
import Test.Hspec
import Test.QuickCheck
import TestSupport (mkExternal, mkExternalWith, mkLocal, valueKey)

spec :: Spec
spec =
  describe "indexCabalFile" $ do
    it "ignores package names outside dependency declarations" $ do
      let contents = unlines
            [ "description:"
            , "  Mentions Agda"
            , "library"
            , "  build-depends: Agda >=2.8"
            ]
          index = indexCabalFile "." "agda2scala.cabal" contents
      fmap (Map.lookup ("agda2scala.cabal", PackageName "Agda")) index `shouldBe` Right (Just 4)

    it "parses multiline dependencies and indexes packages, not sublibraries or executables" $ do
      let contents = unlines
            [ "library"
            , "  build-depends:"
            , "    , foo:{bar,"
            , "           baz} == {1.0, 2.0}"
            , ""
            , "    -- comment"
            , "    , qux >= 1"
            , "    , demo"
            , "      >= 2"
            , "  build-tool-depends:"
            , "    , happy:happy >= 1"
            , "    , alex:alex, example:binary"
            ]
      indexLines contents `shouldBe` Right (Map.fromList
        [ ("foo", 3), ("qux", 7), ("demo", 8)
        , ("happy", 11), ("alex", 12), ("example", 12)
        ])

    it "visits component sections and both conditional branches" $ do
      let contents = unlines
            [ "library"
            , "  if flag(debug)"
            , "    build-depends: debug-pkg"
            , "  else"
            , "    build-depends: release-pkg"
            , "executable app"
            , "  build-depends: app-pkg"
            , "test-suite tests"
            , "  build-depends: test-pkg"
            , "benchmark bench"
            , "  build-depends: bench-pkg"
            , "foreign-library ffi"
            , "  build-depends: ffi-pkg"
            , "custom-setup"
            , "  setup-depends: setup-pkg"
            , "  build-depends: ignored-setup"
            , "source-repository head"
            , "  build-depends: ignored-repository"
            ]
      indexLines contents `shouldBe` Right (Map.fromList
        [ ("debug-pkg", 3), ("release-pkg", 5), ("app-pkg", 7)
        , ("test-pkg", 9), ("bench-pkg", 11), ("ffi-pkg", 13), ("setup-pkg", 15)
        ])

    it "also visits elif branches" $ do
      let contents = unlines
            [ "library"
            , "  if flag(one)"
            , "    build-depends: first-pkg"
            , "  elif flag(two)"
            , "    build-depends: second-pkg"
            , "  else"
            , "    build-depends: third-pkg"
            ]
      indexLines contents `shouldBe` Right (Map.fromList
        [("first-pkg", 3), ("second-pkg", 5), ("third-pkg", 7)])

    it "follows common-stanza imports, including imports in conditionals" $ do
      let contents = unlines
            [ "common unused"
            , "  build-depends: unused-pkg"
            , "common base-settings"
            , "  build-depends: shared-pkg"
            , "common extra-settings"
            , "  import: base-settings"
            , "  build-tool-depends: tool-pkg:tool"
            , "library"
            , "  if flag(debug)"
            , "    import: extra-settings"
            , "    build-depends: local-pkg"
            , "  else"
            , "    build-depends: alternate-pkg"
            ]
      indexLines contents `shouldBe` Right (Map.fromList
        [ ("shared-pkg", 4), ("tool-pkg", 7), ("local-pkg", 11), ("alternate-pkg", 13) ])

    it "indexes repeated and diamond-shaped imports without repeated expansion" $ do
      let common n =
            [ "common shared" <> show n
            , "  import: shared" <> show (n - 1) <> ", shared" <> show (n - 1)
            ]
          contents = unlines $
            ["common shared0", "  build-depends: shared-pkg"]
              <> concatMap common [1 :: Int .. 25]
              <> ["library", "  import: shared25"]
      indexLines contents `shouldBe` Right (Map.singleton "shared-pkg" 2)

    it "ignores imports after other fields, as Cabal does" $ do
      let contents = unlines
            [ "common unused"
            , "  build-depends: unused-pkg"
            , "library"
            , "  build-depends: used-pkg"
            , "  import: unused"
            ]
      indexLines contents `shouldBe` Right (Map.singleton "used-pkg" 4)

    it "supports old sectionless Cabal files" $ do
      indexLines (unlines ["name: old", "build-depends: base, text"])
        `shouldBe` Right (Map.fromList [("base", 2), ("text", 2)])

    it "reports malformed dependency fields and undefined imports" $ do
      indexLines (unlines ["library", "  build-depends: base >="]) `shouldSatisfy` isLeft
      indexLines (unlines ["library", "  import: missing"]) `shouldSatisfy` isLeft
      indexLines (unlines ["library", "  import: later", "common later", "  build-depends: base"])
        `shouldSatisfy` isLeft

    it "locates the reported indirect process finding at Agda in agda2scala" $ do
      let fixtureDir = "test/golden/cabal-file-index"
          root = (mkLocal "agda2scala" [UnitId "Agda-2.8.0.2"])
            { rawPlanItemPkgName = Just (PackageName "agda2scala")
            , rawPlanItemPkgSrc = Just (RawPkgSrc (Just "local") (Just fixtureDir))
            }
          graph = extractPlanGraph $ RawPlan Nothing Nothing
            [ root
            , mkExternalWith "Agda-2.8.0.2" "Agda" "2.8.0.2" [UnitId "process-1.6.18.0"]
            , mkExternal "process-1.6.18.0" "process" "1.6.18.0"
            ]
          sarif = Aeson.object
            [ "runs" Aeson..= [Aeson.object
              [ "results" Aeson..= [Aeson.object
                [ "ruleId" Aeson..= ("HSEC-2024-0003" :: Text.Text)
                , "message" Aeson..= Aeson.object ["text" Aeson..= ("advisory\n(process)" :: Text.Text)]
                ]]
              ]]
            ]
      index <- buildCabalLineIndex fixtureDir graph
      let enriched = enrichSarifValue index AllLocalUnits graph sarif
          result = firstArray (valueKey "results" (firstArray (valueKey "runs" enriched)))
          location = firstArray (valueKey "locations" result)
          region = valueKey "region" (valueKey "physicalLocation" location)
      valueKey "startLine" region `shouldBe` Aeson.Number 67

    it "selects the earliest declaration for every generated dependency" $
      property prop_earliestDeclarations

    it "shifts the selected line when comments are inserted before declarations" $
      property prop_commentsShiftLines

    -- Upstream fixtures at agda2scala@117ded67 and persistent@52651fd5.
    goldenIndex "agda2scala" "agda2scala.cabal"
    goldenIndex "persistent-mongoDB" "persistent-mongoDB.cabal"

indexLines :: String -> Either String (Map Text.Text Int)
indexLines = fmap (Map.mapKeys (unPackageName . snd)) . indexCabalFile "." "fixture.cabal"

prop_earliestDeclarations :: [NonNegative Int] -> Bool
prop_earliestDeclarations choices =
  indexLines contents == Right expected
 where
  names = map (nameFor . getNonNegative) choices
  nameFor n = ["Agda", "aeson", "bytestring", "foo-bar", "text"] !! (n `mod` 5)
  contents = unlines $
    [ "description: Agda aeson bytestring foo-bar text"
    , "library"
    , "  other-modules: Ignore.These.Names"
    , "  build-depends:"
    ] <> map ("    , " <>) names
  expected = Map.fromList
    [ (Text.pack name, 5 + line)
    | name <- Set.toList (Set.fromList names)
    , Just line <- [elemIndex name names]
    ]

prop_commentsShiftLines :: NonNegative Int -> Bool
prop_commentsShiftLines (NonNegative count) =
  indexLines contents == Right (Map.singleton "aeson" (shift + 2))
 where
  shift = count `mod` 40
  contents = unlines $
    replicate shift "-- aeson" <> ["library", "  build-depends: aeson", "  build-depends: aeson"]

goldenIndex :: String -> FilePath -> Spec
goldenIndex fixtureName fileName =
  it ("matches dependency locations in the real " <> fixtureName <> " file") $ do
    let fixtureDir = "test/golden/cabal-file-index/"
    contents <- readFile (fixtureDir <> fileName)
    expected <- readFile (fixtureDir <> fixtureName <> ".golden")
    case indexLines contents of
      Left err -> expectationFailure err
      Right actual -> renderIndex actual `shouldBe` expected

renderIndex :: Map Text.Text Int -> String
renderIndex = unlines . map renderEntry . Map.toAscList
 where
  renderEntry (name, line) = Text.unpack name <> " " <> show line

firstArray :: Aeson.Value -> Aeson.Value
firstArray (Aeson.Array values) = fromMaybe (error "expected non-empty array") (values Vector.!? 0)
firstArray _ = error "expected array"

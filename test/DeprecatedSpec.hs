{-# LANGUAGE OverloadedStrings #-}

module DeprecatedSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Hgs.Deprecated
  ( DeprecatedPackage(..)
  , Deprecation(..)
  , FailOnDeprecated(..)
  , findDeprecatedPackages
  , renderDeprecatedPackages
  , shouldFailOnDeprecated
  )
import Hgs.Domain
  ( PackageName(..)
  , RawPkgSrc(..)
  , RawPlan(..)
  , RawPlanItem(..)
  , UnitId(..)
  , Version(..)
  )
import Hgs.Extract (extractPlanGraph)
import Test.Hspec
import TestSupport (simpleGraph)

spec :: Spec
spec = do
  describe "findDeprecatedPackages" $ do
    it "reports deprecated external packages" $ do
      let index =
            Map.fromList
              [ ( PackageName "aeson"
                , Deprecation
                    { deprecationPackage = PackageName "aeson"
                    , deprecationReplacements = [PackageName "json-future"]
                    , deprecationReason = Nothing
                    }
                )
              ]

      fmap deprecatedNameVersion (findDeprecatedPackages index simpleGraph)
        `shouldBe` [("aeson", "2.2.4.1")]

    it "renders replacement when available" $ do
      let index =
            Map.fromList
              [ ( PackageName "aeson"
                , Deprecation
                    { deprecationPackage = PackageName "aeson"
                    , deprecationReplacements = [PackageName "json-future"]
                    , deprecationReason = Nothing
                    }
                )
              ]

      renderDeprecatedPackages (findDeprecatedPackages index simpleGraph)
        `shouldContain` "replacement: json-future"

    it "renders multiple replacements when available" $ do
      let index =
            Map.fromList
              [ ( PackageName "aeson"
                , Deprecation
                    { deprecationPackage = PackageName "aeson"
                    , deprecationReplacements =
                        [ PackageName "json-a"
                        , PackageName "json-b"
                        ]
                    , deprecationReason = Nothing
                    }
                )
              ]

      let output = renderDeprecatedPackages (findDeprecatedPackages index simpleGraph)

      output `shouldContain` "replacements:"
      output `shouldContain` "- json-a"
      output `shouldContain` "- json-b"
  describe "shouldFailOnDeprecated" $ do
    it "does not fail by default" $ do
      let deps = findDeprecatedPackages deprecatedAeson simpleGraph
      shouldFailOnDeprecated FailOnNone deps `shouldBe` False

    it "fails on any deprecated dependency with FailOnAny" $ do
      let deps = findDeprecatedPackages deprecatedAeson simpleGraph
      shouldFailOnDeprecated FailOnAny deps `shouldBe` True

    it "fails on direct deprecated dependency with FailOnDirect" $ do
      let deps = findDeprecatedPackages deprecatedAeson simpleGraph
      shouldFailOnDeprecated FailOnDirect deps `shouldBe` True

    it "does not fail on indirect deprecated dependency with FailOnDirect" $ do
      let deps = findDeprecatedPackages deprecatedBytestring simpleGraph
      shouldFailOnDeprecated FailOnDirect deps `shouldBe` False

    it "reports deprecated dependency reachable through component dependencies" $ do
      let graph =
            extractPlanGraph
              RawPlan
                { rawPlanCabalVersion = Nothing
                , rawPlanCompilerId = Nothing
                , rawPlanItems =
                    [ RawPlanItem
                        { rawPlanItemType = Just "configured"
                        , rawPlanItemId = Just (UnitId "unix-time-0.4.17-inplace")
                        , rawPlanItemPkgName = Just (PackageName "unix-time")
                        , rawPlanItemPkgVersion = Just (Version "0.4.17")
                        , rawPlanItemDepends = [UnitId "old-time-1.1.1.0-oldtimehash"]
                        , rawPlanItemPkgSrc = Just (RawPkgSrc (Just "local") (Just "."))
                        }
                    , RawPlanItem
                        { rawPlanItemType = Just "configured"
                        , rawPlanItemId = Just (UnitId "old-time-1.1.1.0-oldtimehash")
                        , rawPlanItemPkgName = Just (PackageName "old-time")
                        , rawPlanItemPkgVersion = Just (Version "1.1.1.0")
                        , rawPlanItemDepends = []
                        , rawPlanItemPkgSrc = Nothing
                        }
                    ]
                }

          index =
            Map.fromList
              [ ( PackageName "old-time"
                , Deprecation
                    { deprecationPackage = PackageName "old-time"
                    , deprecationReplacements = [PackageName "time"]
                    , deprecationReason = Nothing
                    }
                )
              ]

      renderDeprecatedPackages (findDeprecatedPackages index graph)
        `shouldContain` "unix-time-0.4.17 -> old-time-1.1.1.0"

deprecatedNameVersion :: DeprecatedPackage -> (String, String)
deprecatedNameVersion dep =
  ( showPackageName (deprecatedPackageName dep)
  , showVersion (deprecatedPackageVersion dep)
  )

showPackageName :: PackageName -> String
showPackageName (PackageName name) =
  Text.unpack name

showVersion :: Version -> String
showVersion (Version version) =
  Text.unpack version

deprecatedAeson :: Map.Map PackageName Deprecation
deprecatedAeson =
  Map.fromList
    [ ( PackageName "aeson"
      , Deprecation
          { deprecationPackage = PackageName "aeson"
          , deprecationReplacements = [PackageName "json-future"]
          , deprecationReason = Nothing
          }
      )
    ]

deprecatedBytestring :: Map.Map PackageName Deprecation
deprecatedBytestring =
  Map.fromList
    [ ( PackageName "bytestring"
      , Deprecation
          { deprecationPackage = PackageName "bytestring"
          , deprecationReplacements = []
          , deprecationReason = Nothing
          }
      )
    ]
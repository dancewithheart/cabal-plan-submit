{-# LANGUAGE OverloadedStrings #-}

module DeprecatedSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Hgs.Deprecated
  ( DeprecatedPackage(..)
  , Deprecation(..)
  , findDeprecatedPackages
  , renderDeprecatedPackages
  )
import Hgs.Domain
  ( PackageName(..)
  , Version(..)
  )
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
                    , deprecationReplacement = Just (PackageName "json-future")
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
                    , deprecationReplacement = Just (PackageName "json-future")
                    , deprecationReason = Nothing
                    }
                )
              ]

      renderDeprecatedPackages (findDeprecatedPackages index simpleGraph)
        `shouldContain` "replacement: json-future"

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

{-# LANGUAGE OverloadedStrings #-}

module LocalsSpec (spec) where

import Hgs.Locals
  ( inspectLocals
  , renderLocals
  )
import Test.Hspec
import TestSupport (simpleGraph)

spec :: Spec
spec = do
  describe "inspectLocals" $ do
    it "finds local packages" $ do
      length (inspectLocals simpleGraph) `shouldBe` 1

    it "finds direct external dependencies of local packages" $ do
      let output = renderLocals (inspectLocals simpleGraph)

      output `shouldContain` "local packages:"
      output `shouldContain` "root-0.1.0.0"
      output `shouldContain` "aeson-2.2.4.1"
      output `shouldContain` "text-2.0.2"
      output `shouldNotContain` "bytestring-0.11.5.3"

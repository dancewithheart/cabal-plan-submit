{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Hgs.Domain (rawPlanCompilerId, rawPlanItems)
import Hgs.Input.PlanJson (decodeRawPlan)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "decodeRawPlan" $ do
    it "parses a minimal plan with one install-plan entry" $ do
      let input =
            "{\
            \  \"cabal-lib-version\": \"3.10.1.0\",\
            \  \"compiler-id\": \"ghc-9.8.2\",\
            \  \"install-plan\": [\
            \    {\
            \      \"type\": \"pre-existing\",\
            \      \"id\": \"base-4.19.1.0\",\
            \      \"pkg-name\": \"base\",\
            \      \"pkg-version\": \"4.19.1.0\",\
            \      \"depends\": []\
            \    }\
            \  ]\
            \}"
      case decodeRawPlan input of
        Left err ->
          expectationFailure err
        Right plan -> do
          length (rawPlanItems plan) `shouldBe` 1
          rawPlanCompilerId plan `shouldBe` Just "ghc-9.8.2"

    it "tolerates missing optional fields" $ do
      let input =
            "{\
            \  \"install-plan\": [\
            \    {\
            \      \"id\": \"bytestring-0.12.1.0\"\
            \    }\
            \  ]\
            \}"
      case decodeRawPlan input of
        Left err ->
          expectationFailure err
        Right plan ->
          length (rawPlanItems plan) `shouldBe` 1

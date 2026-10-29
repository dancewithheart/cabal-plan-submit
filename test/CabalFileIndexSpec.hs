{-# LANGUAGE OverloadedStrings #-}

module CabalFileIndexSpec (spec) where

import Data.Map.Strict qualified as Map
import Hgs.CabalFileIndex (indexCabalFile)
import Hgs.Domain (PackageName(..))
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  describe "indexCabalFile" $ do
    it "records the first non-comment occurrence" $
      property prop_firstOccurrence

prop_firstOccurrence :: Positive Int -> Bool
prop_firstOccurrence (Positive commentCount) =
  Map.lookup ("mypkg.cabal", PackageName "aeson") index == Just (commentCount + 1)
 where
  contents =
    unlines $
      replicate commentCount "-- aeson" <> [ "build-depends: aeson", "build-depends: aeson" ]
  index = indexCabalFile "." "mypkg.cabal" contents

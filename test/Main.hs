module Main (main) where

import Test.Hspec qualified as Hspec
import PlanJsonSpec qualified
import SnapshotSpec qualified

main :: IO ()
main =
  Hspec.hspec $ do
    PlanJsonSpec.spec
    SnapshotSpec.spec

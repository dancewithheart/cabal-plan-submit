module Main (main) where

import Test.Hspec qualified as Hspec
import DeprecatedSpec qualified
import PlanJsonSpec qualified
import SnapshotSpec qualified
import LocalsSpec qualified
import SarifEnrichSpec qualified
import GoldenSpec qualified

main :: IO ()
main =
  Hspec.hspec $ do
    PlanJsonSpec.spec
    SnapshotSpec.spec
    DeprecatedSpec.spec
    LocalsSpec.spec
    SarifEnrichSpec.spec
    GoldenSpec.spec

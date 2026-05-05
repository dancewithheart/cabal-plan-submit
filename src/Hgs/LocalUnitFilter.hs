{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Hgs.LocalUnitFilter
  ( LocalUnitFilter(..)
  , localUnitAllowed
  , isTestLikeUnitId
  , isBenchLikeUnitId
  ) where

import Data.Text qualified as Text
import Hgs.Domain
  ( Package(..)
  , UnitId(..)
  )

data LocalUnitFilter
  = AllLocalUnits
  | ProductionLocalUnits
  deriving stock (Eq, Show)

localUnitAllowed :: LocalUnitFilter -> Package -> Bool
localUnitAllowed filterKind pkg =
  case filterKind of
    AllLocalUnits ->
      True

    ProductionLocalUnits ->
      not (isTestLikeUnitId unitId || isBenchLikeUnitId unitId)
 where
  unitId =
    packageUnitId pkg

isTestLikeUnitId :: UnitId -> Bool
isTestLikeUnitId (UnitId unitId) =
  any (`Text.isInfixOf` unitId)
    [ "-test"
    , "-spec"
    , "-specs"
    ]

isBenchLikeUnitId :: UnitId -> Bool
isBenchLikeUnitId (UnitId unitId) =
  "-bench" `Text.isInfixOf` unitId

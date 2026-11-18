module Hgs.Reachability
  ( closure
  , reachableUnits
  ) where

import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Hgs.Domain (PlanGraph(..), UnitId, packageDepends)

-- The least fixed point of the successor relation, including the seeds.
closure :: Ord a => (a -> Set a) -> Set a -> Set a
closure successors = go Set.empty
 where
  go seen frontier
    | Set.null frontier = seen
    | otherwise =
        let seen' = Set.union seen frontier
            next = Set.unions (map successors (Set.toList frontier)) Set.\\ seen'
         in go seen' next

reachableUnits :: PlanGraph -> Set UnitId
reachableUnits graph = closure successors (planGraphLocals graph)
 where
  successors unit = maybe Set.empty packageDepends (Map.lookup unit (planGraphPackages graph))

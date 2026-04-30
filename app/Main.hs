module Main (main) where

import Hgs.Domain (RawPlan)
import Hgs.Extract (extractPlanGraph, summarisePlanGraph)
import Hgs.Input.PlanJson (readRawPlan, summariseRawPlan)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["inspect-plan", path] ->
      inspectPlan path
    ["inspect-graph", path] ->
      inspectGraph path
    _ ->
      die usage

inspectPlan :: FilePath -> IO ()
inspectPlan path = do
  plan <- readPlanOrDie path
  putStrLn (summariseRawPlan plan)

inspectGraph :: FilePath -> IO ()
inspectGraph path = do
  plan <- readPlanOrDie path
  putStrLn (summarisePlanGraph (extractPlanGraph plan))

readPlanOrDie :: FilePath -> IO Hgs.Domain.RawPlan
readPlanOrDie path = do
  exists <- doesFileExist path
  if not exists
    then die (missingPlanMessage path)
    else do
      ePlan <- readRawPlan path
      case ePlan of
        Left err ->
          die ("failed to parse plan.json: " <> err)
        Right plan ->
          pure plan

missingPlanMessage :: FilePath -> String
missingPlanMessage path =
  unlines
    [ "plan.json not found: " <> path
    , ""
    , "Expected input is Cabal's build plan file."
    , "Usually you need to run this in the target project first:"
    , "  cabal build all"
    , ""
    , "Then try again with:"
    , "  cabal-plan-submit inspect-plan dist-newstyle/cache/plan.json"
    ]

usage :: String
usage =
  unlines
    [ "Usage:"
    , "  cabal-plan-submit inspect-plan  PATH_TO_PLAN_JSON"
    , "  cabal-plan-submit inspect-graph PATH_TO_PLAN_JSON"
    ]

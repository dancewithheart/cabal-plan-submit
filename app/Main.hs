module Main (main) where

import Hgs.Input.PlanJson (readRawPlan, summariseRawPlan)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["inspect-plan", path] ->
      inspectPlan path
    _ ->
      die usage

inspectPlan :: FilePath -> IO ()
inspectPlan path = do
  ePlan <- readRawPlan path
  case ePlan of
    Left err ->
      die ("failed to parse plan.json: " <> err)
    Right plan ->
      putStrLn (summariseRawPlan plan)

usage :: String
usage =
  unlines
    [ "Usage:"
    , "  cabal-plan-to-github-snapshot inspect-plan PATH_TO_PLAN_JSON"
    ]

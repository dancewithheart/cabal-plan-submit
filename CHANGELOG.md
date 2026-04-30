# Revision history for cabal-plan-submit

## 0.1.0.0 -- 2026-04-30

* Initial MVP.
* Add `inspect-plan` for reading and summarising Cabal `plan.json`.
* Parse minimal `plan.json` fields required for later dependency graph extraction.
* Add parser tests for minimal and partially missing plan entries.

## 0.1.0.1 -- 2026-05-01

* Add `inspect-graph` for extracting and summarising a package dependency graph.
* Classify packages as local vs external from `pkg-src.type == "local"`.
* Mark direct external dependencies of local packages.
* Filter dependency edges to known package ids only.
* Add graph tests, including a property that extracted dependency edges only point to known package ids.
* Improve missing `plan.json` error with a hint to run `cabal build all`.

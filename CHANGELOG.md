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

## 0.1.0.2 -- 2026-05-01

* Add `render-snapshot` for producing GitHub dependency submission snapshot JSON.
* Emit one synthetic `cabal-project` manifest containing resolved external Hackage packages.
* Encode package dependencies as `pkg:hackage/<name>@<version>` PURLs.
* Set detector metadata for `cabal-plan-submit`.
* Improve README examples to use real `sha` / `ref` values and avoid Cabal build logs in redirected snapshot output.

## 0.1.0.3 -- 2026-05-01

* Add `validate-snapshot` command for local snapshot sanity checks.
* Validate required GitHub snapshot fields before submission.
* Check that manifests contain resolved dependency objects.
* Check resolved dependency closure: every dependency reference must point to another resolved package.
* Check that packages do not list their own `package_url` as a dependency.
* Check for duplicate dependency entries per package.
* Print an explicit validation report showing which checks were run.

## 0.1.0.4 -- 2026-05-01

* Add `--help` and `--version`.
* Use Cabal package version as the detector version in generated snapshots.
* Add `inspect-deprecated` for reporting deprecated Hackage packages from `deprecated.yaml`.
* Support `commercialhaskell/all-cabal-metadata` `deprecated.yaml` list format.

## 0.1.0.5 -- 2026-05-01

* Add `why` command for explaining why a package appears in the Cabal dependency graph.
* Show shortest dependency paths from local packages to a target package.
* Extend `inspect-deprecated` with `used by path` output for each deprecated package.
* Make deprecated-package reports actionable by showing which local package chain pulls in the deprecated dependency.
* Add support for `commercialhaskell/all-cabal-metadata` `deprecated.yaml` list format.


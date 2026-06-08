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

## 0.1.0.6 -- 2026-05-01

* Add deprecated Hackage package detection from `commercialhaskell/all-cabal-metadata` `deprecated.yaml`.
* Add support for plural replacement packages from `in-favour-of`.
* Add `why` command for explaining why a package appears in the Cabal dependency graph.
* Show dependency paths from local packages to target packages.
* Extend `inspect-deprecated` with `used by path` output, making deprecated dependency reports actionable.
* Report direct vs indirect relationship for deprecated packages.

## 0.1.0.7 -- 2026-05-01
* update example yaml for Github Actions.
* move Aeson instances to fix warning.

## 0.1.0.8 -- 2026-05-01
* fix for empty file.source_location.
* Add `inspect-deprecated --fail-on none|direct|any`.
* Add `inspect-locals` to list locally defined stanza.

## 0.1.0.9 -- 2026-05-30

* Add `enrich-sarif` for augmenting `cabal-audit` SARIF output with Cabal solved-plan information. ([#16](https://github.com/dancewithheart/cabal-plan-submit/pull/16))
* Add dependency-path explanations to SARIF messages and properties.
* Classify SARIF findings as direct or transitive dependencies.
* Add SARIF tags such as `direct-dependency` and `transitive-dependency`.
* Rewrite placeholder project-root SARIF locations to repo-relative `.cabal` files.
* Add precise `.cabal` line locations for the first local dependency on each dependency path.
* Lower SARIF level / severity for transitive dependency findings. ([#19](https://github.com/dancewithheart/cabal-plan-submit/pull/19))
* Add `deprecated-sarif` for emitting SARIF results for deprecated Hackage packages. ([#17](https://github.com/dancewithheart/cabal-plan-submit/pull/17))
* Include deprecated package replacements, dependency paths, direct/transitive relationship, and precise `.cabal` locations in generated SARIF.
* Support `--production-only` and `--ignore-package` for deprecated SARIF output. ([#9](https://github.com/dancewithheart/cabal-plan-submit/pull/9))
* Add name of the deprecated library in issue title. ([#23](https://github.com/dancewithheart/cabal-plan-submit/issues/23))

## 0.1.1.0 -- 2026-06-08

* Add `-why-tree` reverse tree of paths to dependency. ([#36](https://github.com/dancewithheart/cabal-plan-submit/pull/36))
* Allow to use `cabal plan-submit xyz` using cabal external commands. ([#39](https://github.com/dancewithheart/cabal-plan-submit/pull/39))
* More verbose errors on wrong CLI arguments.
* Add alias deprecated to inspect-deprecated.
* Add zizmor for CI. ([#33](https://github.com/dancewithheart/cabal-plan-submit/pull/33))
* Add golden tests. ([#37](https://github.com/dancewithheart/cabal-plan-submit/pull/37))

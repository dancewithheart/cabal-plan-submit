# cabal-plan-submit

Haskell dependency intelligence for supply-chain security.
- submit Cabal plan dependencies to GitHub Dependency Graph
- explain dependency paths with `why`
- classify direct vs transitive dependencies
- inspect deprecated Hackage packages
- enrich cabal-audit SARIF with locations and dependency-path metadata

## What it does

`cabal-plan-submit` reads Cabal's `dist-newstyle/cache/plan.json` and helps with three practical workflows:

1. Find deprecated Hackage dependencies and explain why they are present.

```sh
cabal run cabal-plan-submit -- why dist-newstyle/cache/plan.json cryptonite
cabal run cabal-plan-submit -- why $PROJECT_PATH/dist-newstyle/cache/plan.json process
```
2. Submit Cabal dependency graphs to GitHub Dependency Graph.
```sh
cabal-plan-submit render-snapshot dist-newstyle/cache/plan.json "$SHA" "$REF" > snapshot.json
```

## Use in Github Action

To run this action on Github you can re-use config ([.github/workflows/submit_deps.yml](.github/workflows/submit_deps.yml)):

After successful build data should apperar in `Insights` -> `Dependency graph` in `ecosystem:other`
e.g. as [in here](https://github.com/dancewithheart/agda2scala/network/dependencies?q=ecosystem%3Aother)

You can export SBOM (Software Bill of Materials) file on `Dependency graph` tab.

Fail CI on deprecated dependencies:

```sh
cabal-plan-submit inspect-deprecated --fail-on direct dist-newstyle/cache/plan.json deprecated.yaml
```

## Usage

Inspect local project
```
cabal build
cabal run cabal-plan-submit -- inspect-plan dist-newstyle/cache/plan.json
cabal run cabal-plan-submit -- inspect-graph dist-newstyle/cache/plan.json
```

Inspect another project
```
cabal run cabal-plan-submit -- inspect-plan "$PROJECT_PATH/dist-newstyle/cache/plan.json"
cabal run cabal-plan-submit -- inspect-graph "$PROJECT_PATH/dist-newstyle/cache/plan.json"
```

Render a snapshot JSON file
```
SHA="${GITHUB_SHA:-$(git rev-parse HEAD)}"
REF="${GITHUB_REF:-refs/heads/$(git branch --show-current)}"

cabal build
SNAPSHOT_BIN="$(cabal list-bin exe:cabal-plan-submit)"
"$SNAPSHOT_BIN" render-snapshot dist-newstyle/cache/plan.json "$SHA" "$REF" > snapshot.json
```

Validate snapshot:
```sh
cabal run cabal-plan-submit -- validate-snapshot snapshot.json
```

Explain why a package is present:

```sh
cabal run cabal-plan-submit -- why dist-newstyle/cache/plan.json cryptonite
cabal run cabal-plan-submit -- why "$PROJECT_PATH/dist-newstyle/cache/plan.json" cryptonite
```

## Detecting deprecated packages

```sh
curl -L \
  https://raw.githubusercontent.com/commercialhaskell/all-cabal-metadata/master/deprecated.yaml \
  -o deprecated.yaml

cabal run cabal-plan-submit -- inspect-deprecated dist-newstyle/cache/plan.json deprecated.yaml
cabal run cabal-plan-submit -- inspect-deprecated "$PROJECT_PATH/dist-newstyle/cache/plan.json" deprecated.yaml
```

## Relationship to Renovate / Dependabot / cabal-audit

- `Renovate` can update declared `.cabal` dependency bounds.
- `cabal-audit` checks known Haskell security advisories.
- `cabal-plan-submit` works from Cabal's solved `plan.json`, submits the exact resolved graph to GitHub, and explains deprecated dependencies with paths.

## Working with code

Running tests:

```sh
cabal test
```

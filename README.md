# cabal-plan-submit

Extract a dependency graph for GitHub dependency submission snapshots.

## What it does

`cabal-plan-submit` reads Cabal's `dist-newstyle/cache/plan.json` and helps with three practical workflows:

1. Find deprecated Hackage dependencies and explain why they are present.

```sh
cabal-plan-submit inspect-deprecated dist-newstyle/cache/plan.json deprecated.yaml
cabal-plan-submit why dist-newstyle/cache/plan.json cryptonite
```
2. Submit Cabal dependency graphs to GitHub Dependency Graph.
```sh
cabal-plan-submit render-snapshot dist-newstyle/cache/plan.json "$SHA" "$REF" > snapshot.json
```

## Integration on Github Action

```yaml
name: Dependency submission

on:
  push:
    branches: [master, main]
  pull_request:
  workflow_dispatch:

permissions:
  contents: write

jobs:
  submit-dependencies:
    runs-on: ubuntu-latest

    steps:
      - name: Checkout
        uses: actions/checkout@v6

      - name: Setup Haskell
        uses: haskell-actions/setup@v2
        with:
          ghc-version: "9.6.7"
          cabal-version: "3.14"

      - name: Build project
        run: cabal build all

      - name: Build cabal-plan-submit
        run: cabal build exe:cabal-plan-submit

      - name: Render snapshot
        env:
          SHA: ${{ github.sha }}
          REF: ${{ github.ref }}
        run: |
          BIN="$(cabal list-bin exe:cabal-plan-submit)"
          "$BIN" render-snapshot dist-newstyle/cache/plan.json "$SHA" "$REF" > snapshot.json
          jq '{version, sha, ref, manifests: (.manifests | keys)}' snapshot.json

      - name: Submit snapshot
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
          REPO: ${{ github.repository }}
        run: |
          owner="${REPO%/*}"
          repo="${REPO#*/}"

          curl \
            --fail-with-body \
            -X POST \
            -H "Accept: application/vnd.github+json" \
            -H "Authorization: Bearer $GITHUB_TOKEN" \
            -H "X-GitHub-Api-Version: 2022-11-28" \
            "https://api.github.com/repos/$owner/$repo/dependency-graph/snapshots" \
            --data-binary @snapshot.json
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

## Working with code

Running tests:

```sh
cabal test
```

Manual validation:

Sanity check top-level fields:
```sh
jq '{version, sha, ref, job, detector, scanned, manifests: (.manifests | keys)}' snapshot.json
```

Count resolved packages:
```sh
jq '.manifests["cabal-project"].resolved | length' snapshot.json
```

Show a few resolved entries:
```sh
jq '.manifests["cabal-project"].resolved | to_entries[:5]' snapshot.json
```


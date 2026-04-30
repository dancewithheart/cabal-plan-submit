# cabal-plan-submit

Extract a dependency graph for GitHub dependency submission snapshots.

```sh
cabal test
```

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

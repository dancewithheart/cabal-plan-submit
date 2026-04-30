# cabal-plan-submit

Extract a dependency graph for GitHub dependency submission snapshots.

```sh
cabal test
```

test against `PROJECT_PATH`
```
cabal build
cabal run cabal-plan-submit -- inspect-plan dist-newstyle/cache/plan.json
cabal run cabal-plan-submit -- inspect-plan $PROJECT_PATH/dist-newstyle/cache/plan.json

cabal run cabal-plan-submit -- inspect-graph dist-newstyle/cache/plan.json
cabal run cabal-plan-submit -- inspect-graph  $PROJECT_PATH/dist-newstyle/cache/plan.json
```

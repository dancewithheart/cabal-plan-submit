# cabal-plan-to-github-snapshot

Extract a dependency graph for GitHub dependency submission snapshots.

```sh
cabal test
```

test against `PROJECT_PATH`
```
cabal build
cabal run cabal-plan-to-github-snapshot -- inspect-plan dist-newstyle/cache/plan.json
cabal run cabal-plan-to-github-snapshot -- inspect-plan $PROJECT_PATH/dist-newstyle/cache/plan.json
```

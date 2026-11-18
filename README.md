# cabal-plan-submit

Combine and enhance information about Haskell project:
- vulnerability report from `cabal-audit`
- deprecated libraries

Allows you to:
- explain why dependencies exist
- classify direct vs transitive dependencies
- convert `cabal` resolved plan (`plan.json`) with depndencies to format expected by GitHub and submit it 
to help with supply-chain security esp on GitHub.
- enrich cabal-audit results in SARIF with locations and dependency paths

You can extract SBOM from GH after this with this informations.

## Use cases

Assume you use project as external cabal command:

1. Find deprecated Hackage dependencies and explain why they are present

```sh
curl -L \
  https://raw.githubusercontent.com/commercialhaskell/all-cabal-metadata/master/deprecated.yaml \
  -o deprecated.yaml
cabal plan-submit deprecated
```

```sh
cabal plan-submit why cryptonite
cabal plan-submit why --production-only aeson
```

2. Create and send Cabal dependency graphs to GitHub Dependency Graph (currently GH cannot do this automatically)
```sh
SHA="${GITHUB_SHA:-$(git rev-parse HEAD)}"
REF="${GITHUB_REF:-refs/heads/$(git branch --show-current)}"

cabal run cabal-plan-submit -- render-snapshot dist-newstyle/cache/plan.json "$SHA" "$REF" > snapshot.json
```


3. Improve SARIF file with vulnerabilities created by `cabal-audit` so they are nicely rendered on GitHub and have
extra information: whether dependency is transitive or direct, what paths leads to the dependency

### Use in Github action

Example [.github/workflows/submit_deps.yml](.github/workflows/submit_deps.yml).

After successful build data should appear in `Insights` -> `Dependency graph` in `ecosystem:other`
e.g. as [in here](https://github.com/dancewithheart/agda2scala/network/dependencies?q=ecosystem%3Aother)

You can export SBOM (Software Bill of Materials) file on `Dependency graph` tab.

CI can fail when there are deprecated dependencies:
```sh
cabal-plan-submit deprecated --fail-on direct dist-newstyle/cache/plan.json deprecated.yaml
```

### CLI usage as cabal command

You can download project build it
```sh
cabal install exe:cabal-plan-submit --overwrite-policy=always
```
and run it from directory you downloaded and point to target project:
```sh
cabal run cabal-plan-submit -- why $PROJECT_PATH/dist-newstyle/cache/plan.json cryptonite
cabal run cabal-plan-submit -- deprecated dist-newstyle/cache/plan.json deprecated.yaml
```

or run as [external cabal command](https://discourse.haskell.org/t/an-external-command-system-for-cabal-what-would-you-do-with-it/7114):
```sh
cabal plan-submit why cryptonite
```

## Dev notes

Tests (first command generate golden tests):
```sh
UPDATE_GOLDEN=1 cabal test
cabal test
```

Static code analysis:
```sh
hlint src app test
```

Show diagnostic commands:
```sh
cabal run cabal-plan-submit -- --help-all
```

## Blog posts

* [Blog post](https://dancewithheart.github.io/posts/2026-05-31-cabal-plan-submit.html) explaining example use cases.
* [SBOM generation for Haskell projects](https://github.com/haskell/security-advisories/blob/main/guides/sbom_dependencies.md) - guide on [haskell.org/security](https://www.haskell.org/security/)

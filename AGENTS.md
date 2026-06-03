```
# Repository guidelines

This project is a small Haskell CLI/library for Cabal dependency graph analysis.

Review priorities:

1. Preserve graph correctness.

   * Dependency edges must point to known units.
   * Snapshot dependencies must be closed over resolved packages.
   * No package may depend on its own PURL.
   * Local/test/spec/bench filtering must not silently hide production paths.
   * Reachability, directness, and dependency-path logic should have property tests where practical.

2. Prefer precise parsing over permissive data loss.

   * Missing optional fields are acceptable when the Cabal format allows them.
   * Known Cabal shapes such as component-level `depends` must be handled.
   * If data is ignored intentionally, document why.
   * Prefer explicit `Either`, `Maybe`, or dedicated ADTs for expected parse and validation failures.
   * Never silently convert unknown or malformed data into empty results - explicitly specify errors in types.

3. Use types to make incorrect behavior unrepresentable.

   * Prefer ADTs that split the domain into composable cases which can be reasoned about and tested.
   * Make illegal states unrepresentable.
   * Public functions should use the most precise domain types, awoid raw `Text`/`String`.
   * Use newtypes for package identity, version, unit id, PURL, dependency relationship, local-unit filter, and similar domain concepts.
   * Prefer explicit type signatures for exported functions and non-trivial internal functions.

4. Prefer algebraic and mathematical models over ad-hoc ones.

   * Use well-understood abstractions from functional programming, abstract algebra, category theory, order theory, graph theory, or other mathematics.
   * Prefer abstractions that expose laws, invariants, composability, or property-testable behavior.
   * Type classes are welcome when they encode real structure and useful laws.

5. Prefer property tests over example-only tests.

   * Use property tests when possible.
   * Use unit tests when they document specific behavior or reproduce a concrete regression.
   * Use golden tests to have real examples they serve as acceptance tests.
   * Test names should describe observable behavior clearly so `cabal test` output documents the modules/features being checked.

6. Prefer idiomatic Haskell.

   * Prefer small focused pure functions.
   * Keep IO at the CLI boundary.
   * Prefer standard functional combinators such as `map`, `foldMap`, `traverse`, `foldl'`, `Set`, and `Map` operations when they make the algorithm easier to reason about.
   * Prefer clear, law-friendly code over low-level optimized code unless performance is measured and important.
   * Avoid Python-like or C-like structure, prefer idiomatic Haskell model.

7. Use precise names.

   * Names should reveal domain meaning but remain short.
   * Prefer established terminology from Cabal, Hackage, SARIF, GitHub Dependency Submission, PURL, and Haskell security advisories.
   * Avoid ad-hoc names when a standard term exists.
   * Avoid over-elaborate names that obscure simple concepts.

8. Security/dependency output should be actionable.

   * Show dependency paths when reporting deprecated or vulnerable packages.
   * Distinguish direct vs indirect dependencies.
   * Mark test/spec/bench filtering as heuristic unless component semantics are exact.
   * Prefer output that helps identify the upstream package where a useful PR should be opened.
   * Reduce noise: do not report low-confidence findings as high-confidence failures.
   * When enriching SARIF, preserve original cabal-audit information and add structured metadata rather than destroying existing context.

9. Maintainability matters.

   * Keep subdomains graph, deprecation, SARIF, cli parsing, output pretty printing separate, coherent, composable.
   * Prefer pure functions that can later be extracted into a reusable library.
   * Favor changes that improve correctness, diagnostics, or user actionability over cosmetic churn.
```

# CLAUDE.md

## Rules

- **Only commit and push when explicitly instructed.** Never amend commits. Never add `Co-Authored-By` headers.
- Run `cabal test` and `cabal build --ghc-options="-Werror"` before considering any change complete.
- **`-Wall -Wcompat` clean.** All code compiles without warnings under `-Wall -Wcompat`. Warnings are errors in CI — no exceptions, no suppressions.

## Haskell Style

- **Pure by default.** Everything is a pure function unless it fundamentally cannot be. IO is a composed effect description, not an escape hatch.
- **Total functions only.** Never use `head`, `tail`, `!!`, `fromJust`, `read`, or any partial function. Use pattern matching, `maybe`, `either`, `BS.uncons`, safe alternatives.
- **Strict by default.** Bang patterns on all data fields and accumulators. `Data.Map.Strict` over `Data.Map.Lazy`. Strict `foldl'` over lazy `foldl`.
- **Types encode invariants.** Newtypes for domain concepts. Exhaustive pattern matches. Make illegal states unrepresentable.
- **Named constants.** No magic numbers or hardcoded strings anywhere.
- **No prime-mark variables.** Never use `x'`, `x''`, `s'`, etc. Use descriptive names.
- **Idiomatic patterns.** Guards over nested if/then/else. Pattern matching over boolean checks. Explicit export lists on all modules.

## Context

Nix binary cache protocol library. Pure-first, minimal dependencies.

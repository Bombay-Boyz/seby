# Seby

Seby is a local-first Haskell tool for transparent fundamental analysis of
Indian listed companies. Its design goal is to make every financial result
traceable to its inputs, reporting period, source, and formula.

## Current stage

The repository has completed the pure-domain and reporting foundation and is
building its SQLite persistence layer. It has validated period types, exact
integer money arithmetic, tested financial formulas, a provenance-linked report
model, terminal/Markdown renderers, versioned SQLite schema migrations, and an
atomic fact repository. Live data ingestion and wiring the report model into
`seby analyse` remain later work.

## Build and checks

The supported compiler is GHC 9.14.1 with Cabal 3.14.1.1. Dependencies are
locked in `cabal.project.freeze`.

```sh
cabal build all --enable-tests
cabal test all --test-show-details=direct
fourmolu --mode check $(rg --files -g '*.hs')
hlint src app test
cabal check
```

Read [the project vision](seby-project-vision.md), [the engineering
specification](seby-engineering-spec.md), and [the Haskell engineering
standard](haskell-engineering-standard.md) before extending the system.

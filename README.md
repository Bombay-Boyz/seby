# Seby

Seby is a local-first Haskell tool for transparent fundamental analysis of
Indian listed companies. Its design goal is to make every financial result
traceable to its inputs, reporting period, source, and formula.

## Current stage

The repository now includes pure-domain, reporting, SQLite persistence,
peer-analysis, market-price ingestion, DCF valuation, and Phase 6 analysis.
Phase 6 provides explicit reported, restated, and publication-time views over
immutable fact revisions; provenance-backed dated corporate actions and
cautious split/bonus price adjustments; segment revenue reconciliation and
concentration analysis; and evidence-linked forensic review signals. Reports
can include precomputed Phase 6 notes alongside sourced metrics.

The valuation engine accepts explicit forecasts and assumptions, returns
enterprise value with its forecast and terminal components, evaluates
bear/base/bull cases and discount-rate/terminal-growth sensitivities, and can
solve for implied terminal growth. The Alpha Vantage adapter uses the
documented free compact daily endpoint for raw, unadjusted prices (latest 100
observations per request); adjusted history and company fundamentals are
outside the free endpoint scope. The API key is supplied by the caller and is
excluded from stored provenance. Wiring sourced financial statements and
analysis into the end-to-end `seby analyse` report remains later work.

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

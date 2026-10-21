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
excluded from stored provenance. Statement refresh supports explicit ordered
FMP and Alpha Vantage attempts, with a separate provider symbol for each
source. The source is selected by the user; failed attempts are printed when a
later source succeeds, and all-source failure exits with the reasons. API
coverage and data basis depend on each provider's plan and ticker availability.
Seby does not fetch or scrape exchange pages. Reports include source URLs and payload digests, raw
supported metrics, and compatible derived metrics such as free cash flow and
margins. Conflicting source values are shown separately and reported as a
data issue; derived metrics are omitted when their inputs are ambiguous.

## Build and checks

The supported compiler is GHC 9.14.1 with Cabal 3.14.1.1. Dependencies are
locked in `cabal.project.freeze`.

```sh
cabal build all --enable-tests
cabal test all --test-show-details=direct
cabal bench seby-baseline --enable-benchmarks
fourmolu --mode check $(rg --files -g '*.hs')
hlint src app test
cabal check
```

Read [the project vision](seby-project-vision.md), [the engineering
specification](seby-engineering-spec.md), and [the Haskell engineering
standard](haskell-engineering-standard.md) before extending the system.

## Local workflow

### Performance baseline

Run the synthetic report and SQLite baseline with `cabal bench seby-baseline
--enable-benchmarks`. It covers a 600-fact report, 200 reports covering 24,000
facts, repeated identical SQLite refreshes, a 4 MiB raw payload write, and first
and warm SQLite analyses over 100 annual periods. Fixture setup is excluded,
and reports are fully rendered. Results are machine-specific; the baseline
records database size and request count, but does not measure memory or live
provider latency. See [`benchmarks/BASELINE.md`](benchmarks/BASELINE.md) for the
recorded run and limitations.

The local SQLite database defaults to `seby.sqlite` in the current directory.
Manage a ticker watchlist and maintain the database with:

```sh
seby watchlist add TCS
seby watchlist list
seby watchlist remove TCS
seby db check
seby db vacuum
seby db backup seby-backup.sqlite
```

Watchlist tickers are normalized to uppercase and duplicate additions are
idempotent. `db check` runs SQLite's integrity check; `db vacuum` reclaims
unused database pages. `db backup FILE` creates a consistent SQLite snapshot
that includes committed WAL data; the destination must not already exist. Batch analysis uses the saved watchlist, prints every
successful report, and exits unsuccessfully if any ticker has no usable data
or encounters an error.

### Import sourced financial statements

Prepare a JSON file from an allowed filing source and retain the filing URL.
Imports are stored locally with the exact payload digest and source
publication time. Amounts are integer minor currency units (for example,
₹1,234.56 is `123456` paise). Supported concepts include `revenue`,
`operating_profit`, `net_profit`, `operating_cash_flow`,
`capital_expenditure`, and `gross_profit` (used to calculate gross margin).

```json
{
  "symbol": "TCS",
  "company_name": "Tata Consultancy Services",
  "source_id": "COMPANY_FILINGS",
  "source_uri": "https://example.org/filing.pdf",
  "published_at": "2026-05-01T00:00:00Z",
  "facts": [
    {
      "concept": "revenue",
      "period_start": "2025-04-01",
      "period_end": "2026-03-31",
      "period_type": "annual",
      "currency": "INR",
      "amount_minor": 123456,
      "basis": "consolidated",
      "scope": "group"
    }
  ]
}
```

Then run:

```sh
seby facts import tcs-fy2026.json
seby analyse TCS --basis consolidated --scope group
seby analyse --watchlist --basis consolidated --scope group
```

Statement APIs can be selected in a user-defined order. The first `--source`
is primary; later sources are tried only when an earlier one fails. Keys are
read from the process environment and never stored or printed. For example:

```sh
seby facts refresh TCS --source fmp:TCS.NS --source alpha-vantage:TCS.BSE
seby facts refresh TCS --source fmp:TCS.NS --mode prefer-cache --max-age-hours 24
seby facts refresh TCS --mode offline
seby analyse TCS --basis consolidated --scope group
```

Set `SEBY_FMP_API_KEY` and/or `SEBY_ALPHA_VANTAGE_API_KEY` for the sources you
select. FMP Basic does not provide confirmed Indian financial statement
coverage; check your plan and ticker availability. Alpha Vantage's free quota
is limited and its global fundamentals coverage for a given Indian listing
must be confirmed by the provider. Both adapters retain the original payload;
their standard endpoints do not reliably label standalone versus consolidated,
so refresh records the basis assumption and prints a warning. Verify the basis
before relying on it. The refresh command never stores or prints an API key.
Refresh modes are explicit: `refresh` always calls the selected provider
sequence, `prefer-cache` reuses that primary provider's successful response
when it is within the configured age and falls back to saved facts if a
refresh fails, and `offline` produces a report from local facts without making
network requests. The freshness window defaults to 24 hours.

The importer validates the entire document before writing anything. Failed
imports and incomplete analyses exit nonzero with an error on stderr.

### Portfolio snapshots

`seby portfolio analyse FILE` reads a local JSON snapshot and prints a Markdown
holdings report. Each holding needs `symbol`, `quantity`, `average_cost`,
`market_price`, `currency`, and `price_date`; the document also needs a `source`
and `retrieved_at`, with an optional HTTPS `source_url`. Quantities and prices
are parsed as exact JSON decimals; per-share prices support up to two decimal
places, and snapshots are limited to 8 MiB. The report includes cost basis,
market value, unrealized gain, return on cost, per-currency portfolio weight,
price date, source, retrieval time, and SHA-256 digest of the input snapshot.
Totals and weights are separated by currency; no FX rates,
fees, taxes, dividends, or corporate-action adjustments are inferred.

```json
{
  "source": "broker-export",
  "source_url": "https://broker.example/export",
  "retrieved_at": "2026-10-21T09:30:00Z",
  "positions": [
    {
      "symbol": "TCS",
      "quantity": 10,
      "average_cost": 3200.00,
      "market_price": 3450.50,
      "currency": "INR",
      "price_date": "2026-10-20"
    }
  ]
}
```

```sh
seby portfolio analyse portfolio.json
```

# Seby — Engineering Specification

**Status:** Normative project specification  
**Project:** Seby (`seby`)  
**Domain:** Fundamental analysis of Indian listed equities (NSE/BSE)  
**Implementation language:** Haskell only  
**Primary invocation:** `seby analyse TCS`  
**Initial storage:** SQLite  
**Project licence:** MIT, subject to the licence and attribution rules of every dependency and reused component

The terms **MUST**, **MUST NOT**, **SHOULD**, **SHOULD NOT**, and **MAY** are normative requirements.

---

## 1. Engineering objective

Seby MUST transform sourced, time-bounded financial facts into reproducible analytical results. Every derived value MUST be explainable as a function of identified inputs, a documented formula, and an explicit policy version.

The system is not a ratio scraper. It is a typed financial-data pipeline:

\[
\text{Sources} \rightarrow \text{Raw Facts} \rightarrow \text{Validated Facts}
\rightarrow \text{Normalised Facts} \rightarrow \text{Metrics}
\rightarrow \text{Analysis} \rightarrow \text{Reports}
\]

Each arrow MUST be testable independently. Network and persistence effects MUST NOT be hidden inside financial formulas.

### Core invariants

1. No untraceable financial fact enters a report.
2. No missing, invalid, stale, or non-applicable value is silently converted to zero.
3. No partial function is permitted in production code.
4. No failed refresh may destroy a previously valid observation.
5. No calculation may silently combine incompatible currencies, periods, statement bases, units, or consolidation scopes.
6. No external source is assumed permanent; each source is isolated behind an adapter.
7. No library or repository is adopted solely because it exists or compiles on one developer's machine.
8. No valuation output may conceal its assumptions.
9. No output may state more precision than its inputs and model justify.
10. Reproducibility and correctness take priority over feature count and micro-optimisation.

---

## 2. Supported market and entity scope

### Initial scope

- Indian listed companies on NSE and BSE.
- Equity securities and ordinary company financial statements.
- Annual results first; quarterly and TTM metrics added when source coverage and normalisation are reliable.
- Consolidated statements preferred for group-level analysis; standalone analysis remains selectable and must never be silently mixed with consolidated data.
- Company and security identifiers must be distinct. A ticker is not a permanent identity.

### Required entity model

A company, a listed security, a source-specific symbol, and a corporate-action successor are different concepts. Store stable internal IDs and dated identifier mappings. Symbol changes MUST NOT create a false new company; mergers and demergers MUST NOT be flattened into an invented continuous history.

---

## 3. Data-source policy

Source adapters MUST return source observations and provenance, not precomputed “truth”. Raw inputs must be retained separately from normalised and derived data.

### 3.1 Source hierarchy

Use the highest-quality available source for each fact. The order below is a preference, not permission to disregard data licensing or technical restrictions.

| Priority | Source class | Intended use | Policy |
|---|---|---|---|
| 1 | NSE and BSE official disclosures and filings | Results, filings, corporate actions, issuer/security reference data | Preferred primary source. Respect published access rules, rate limits, terms, and technical controls. |
| 2 | Company annual reports, quarterly results, investor presentations, exchange-hosted XBRL and other official structured disclosures | Statements, notes, segments, auditor opinions, accounting policies | Preserve document URL, publication date, period, statement basis, and extraction method. |
| 3 | Official corporate-action notices and issuer announcements | Splits, bonus issues, rights issues, mergers, demergers, buybacks, share issuance | Use to reconcile per-share and historical series. |
| 4 | Free market-data providers, including Yahoo Finance interfaces where accessible | Price history, adjusted prices, basic reference data, cross-checks | Treat unofficial endpoints as unstable. Isolate them behind adapters and validate coverage and terms before use. |
| 5 | Public financial-screening websites such as Screener.in | Manual cross-checking and research assistance | Do not assume a documented public API. Automated access or scraping requires a terms and stability review. |
| 6 | Local user-supplied files and cached observations | Offline analysis and reproducibility | Mark the original source and import time; validate schema and provenance. |

The exact available endpoints, terms, coverage, and rate limits MUST be verified during implementation. A website being publicly readable does not automatically grant permission for unrestricted automated collection.

### 3.2 Source adapter contract

Each adapter MUST expose a small typed interface equivalent to:

```haskell
data SourceId = SourceId Text
  deriving (Eq, Ord, Show)

data FetchError
  = NetworkFailure Text
  | RateLimited (Maybe UTCTime)
  | SourceUnavailable
  | InvalidPayload Text
  | UnsupportedEntity Text
  deriving (Eq, Show)

data Provenance = Provenance
  { provenanceSource      :: SourceId
  , provenanceUri         :: Maybe URI
  , retrievedAt           :: UTCTime
  , publishedAt           :: Maybe UTCTime
  , payloadDigest         :: Text
  }

data SourceAdapter m = SourceAdapter
  { fetchCompanyFacts :: CompanyId -> m (Either FetchError [RawFact])
  , fetchPriceHistory :: SecurityId -> DateRange -> m (Either FetchError [RawPrice])
  }
```

This is a design sketch, not a requirement to use these exact type names. Production types MUST prevent invalid states where practical and MUST make effectful operations explicit.

### 3.3 Fallback policy

Fallback is a controlled state transition, not silent substitution.

1. Attempt the preferred source within configured timeouts and retry limits.
2. On transient failure, retry only according to bounded backoff and source policy.
3. Try an approved secondary source for the same fact and period.
4. Validate the fallback observation independently.
5. Compare materially overlapping observations; record disagreement.
6. Persist both source observations with provenance.
7. Choose the report value according to an explicit, versioned source-priority rule.
8. Emit a warning if the chosen value is stale, secondary, disputed, or less complete.
9. If no acceptable value exists, return a typed unavailable state. Do not fabricate a value.

A secondary source MUST NOT override an official filing merely because its payload is easier to parse. If the secondary value conflicts with a primary filing, the discrepancy is surfaced for reconciliation.

### 3.4 Retry, timeout, and rate-limit requirements

- All network calls MUST have explicit connection and response timeouts.
- Retries MUST be bounded; no infinite retry loops.
- Retry only failures likely to be transient.
- Honour `Retry-After` where supplied.
- Apply per-host concurrency and request-rate limits.
- Use jittered exponential backoff for eligible transient failures.
- Do not retry permanent schema errors indefinitely.
- Record attempt count, final outcome, and timestamps.
- Tests MUST use fixtures or a local test server, not depend on live provider availability.

---

## 4. Data model and persistence

### 4.1 Storage principles

SQLite is the initial local persistence layer. The database MUST support offline re-analysis, incremental refresh, provenance, and reproducible historical reports.

Keep these layers logically separate:

1. **Raw payloads:** source response or filing bytes when lawful and practical, plus a cryptographic digest.
2. **Raw observations:** extracted values preserving source naming, units, period, and original context.
3. **Canonical facts:** validated and normalised values in a common internal model.
4. **Derived metrics:** calculated values with formula and policy versions.
5. **Analytical outputs:** report snapshots or reproducible input manifests.
6. **Cache metadata:** retrieval time, freshness, source status, and refresh outcome.

Do not overwrite raw facts with adjusted facts. A correction creates a new observation or version and retains the earlier one.

### 4.2 Required entities

At minimum, model:

- `Company`
- `Security`
- `IdentifierMapping` with effective dates
- `Source`
- `Filing`
- `RawPayload`
- `RawFact`
- `CanonicalFact`
- `CorporateAction`
- `DerivedMetric`
- `MetricDefinition` / formula version
- `RefreshRun`
- `ReportSnapshot`

Names are illustrative. The schema MUST be normalised enough to avoid contradictory duplicated facts, but need not be over-engineered.

### 4.3 Fact identity and versioning

A canonical fact's identity should include the company/security as applicable, concept, period, period type, currency/unit, statement basis, consolidation scope, and relevant dimensions such as segment.

The system MUST distinguish:
- Period end from publication/retrieval date.
- Annual, quarterly, and TTM periods.
- Standalone from consolidated statements.
- Reported from restated data.
- Original source values from normalised values.
- Observed from estimated or derived values.
- Missing from zero.
- Not applicable from not retrieved.
- Non-comparable from valid but poor performance.

### 4.4 Refresh and transaction semantics

- Fetch and validate outside the final write transaction where possible.
- Commit a complete validated batch atomically.
- A failed or partial refresh MUST NOT erase a previously valid snapshot.
- Use schema migrations with explicit versions.
- Enforce primary keys, foreign keys, uniqueness constraints, and suitable checks.
- Use prepared statements / parameterised queries only.
- Configure SQLite pragmas deliberately and document the durability trade-off; do not disable durability merely to improve benchmark results.
- Use WAL mode if it improves the expected local workload and its behaviour is tested.
- Use `EXPLAIN QUERY PLAN` before adding speculative indexes.
- Back up or export the database using SQLite-supported mechanisms, not by copying a live database file without accounting for WAL state.

### 4.5 Cache freshness

Freshness is source- and data-class-specific. Price history and annual filings do not have the same update cadence.

Each cache entry MUST expose:
- Last successful retrieval.
- Last attempted retrieval.
- Source and payload digest.
- Status of last refresh.
- Freshness policy and whether the observation is stale.
- Whether a fallback source supplied the value.

A stale value may be usable if clearly marked and if the report policy permits it. Staleness must never be invisible.

### 4.6 Reuse of collected data

- Reuse validated local data before making a network request.
- Refresh incrementally using stable identifiers and period keys.
- Deduplicate equivalent observations without deleting provenance.
- Cache immutable raw payloads by content digest where appropriate.
- Recompute derived metrics when input facts, formula versions, or normalisation rules change.
- Store formula/version metadata so old reports can be reproduced.
- Support an explicit refresh mode and an offline mode.
- Never treat an HTTP success status as proof that the payload is semantically valid.

---

## 5. Canonical financial model

### 5.1 Strongly typed dimensions

Production code SHOULD represent the following with dedicated types or well-defined newtypes:

- Currency and monetary amount.
- Share count and per-share amount.
- Ratio, percentage, and multiple.
- Fiscal period and period type.
- Consolidation scope.
- Statement basis.
- Source and provenance.
- Observation quality/status.
- Company, security, and segment identity.

Do not use bare `Double` for all financial quantities. Dimensional distinctions must be preserved until an explicit conversion or formula combines them.

### 5.2 Missing and invalid data

Use an algebraic data type instead of `Maybe` alone where the reason matters:

```haskell
data StatementBasis = Standalone | Consolidated
  deriving (Eq, Ord, Show, Enum, Bounded)

data DataIssue
  = NotDisclosed
  | NotRetrieved
  | NotApplicable
  | NotMeaningful Text  -- e.g. CAGR across non-positive values, negative P/E where ratio is not economically meaningful
  | InvalidInput Text
  | NonComparable Text
  | StaleObservation UTCTime
  | ConflictingSources [SourceId]
  deriving (Eq, Show)

data Observed a
  = Available
      { observedValue      :: a
      , observedProvenance :: NonEmpty Provenance
      }
  | Unavailable DataIssue
  deriving (Eq, Show)
```

This is a sketch. Avoid making `Observed` so generic that invalid combinations become possible; domain-specific types may be preferable.

### 5.3 Mathematical calculation policy

Every metric definition MUST specify:
- Mathematical formula.
- Required inputs and units.
- Period alignment rules.
- Treatment of zero and negative denominators.
- Whether averages or period-end balances are used.
- Rounding/display policy.
- Missing-input policy.
- Industry applicability.
- Source and formula version.

Example:

\[
\text{FCF} = \text{Operating Cash Flow} - \text{Capital Expenditure}
\]

The formula is valid only after confirming compatible periods, currencies, and sign conventions. If either input is unavailable or incomparable, FCF is unavailable; it is not zero.

### 5.4 No partial functions

The following are prohibited in production paths unless protected by a locally proven invariant and wrapped behind a total API; in ordinary application code they SHOULD be avoided entirely:

- `head`, `tail`, `last`, `init`
- `fromJust`
- `!!`
- unchecked `read`
- partial pattern matches
- division without explicit denominator validation
- unsafe coercions and unexplained `error` / `undefined`
- assumptions that a list, map lookup, API field, or database result must exist

Use total alternatives:
- Pattern matching on `[]` and `x : xs`.
- `NonEmpty` when non-emptiness is a real invariant.
- `Map.lookup` / `IntMap.lookup` with explicit handling.
- `readMaybe` or a validating parser.
- `Either` / typed error values.
- `traverse`, `sequence`, and validation types for structured errors.
- Explicit checked division returning a typed result.

Compiler warnings for incomplete patterns and incomplete record updates MUST be enabled and treated as errors in CI.

---

## 6. Architecture standards — best practices only

### 6.1 Dependency direction

Use a ports-and-adapters / functional-core, imperative-shell architecture:

```text
CLI / Commands
      |
Application use cases
      |
Domain types + pure financial rules
      ^
      |
Ports / interfaces
      ^
      |
Adapters: HTTP, filing parsers, SQLite, clock, filesystem
```

The domain and financial calculations MUST NOT import concrete HTTP, SQLite, terminal, or provider modules. Adapters depend inward on domain contracts, not the reverse.

### 6.2 Module boundaries

Recommended logical packages/modules:

```text
Seby.Domain
Seby.Domain.Types
Seby.Domain.Period
Seby.Domain.Money
Seby.Domain.Quality
Seby.Ingestion
Seby.Ingestion.NSE
Seby.Ingestion.BSE
Seby.Ingestion.MarketData
Seby.Normalise
Seby.Persistence.SQLite
Seby.Cache
Seby.Metrics
Seby.Analysis
Seby.Peers
Seby.Valuation
Seby.Reporting.Terminal
Seby.Reporting.Markdown
Seby.CLI
Seby.App
```

Do not create empty abstractions solely to mirror this list. Introduce a boundary when it isolates effects, a changing external contract, or a meaningful domain concept.

### 6.3 Effects

- Pure financial functions SHOULD be referentially transparent.
- Network, database, clock, and filesystem effects MUST be visible in types and module boundaries.
- Avoid global mutable state.
- Avoid hidden network access during rendering or metric calculation.
- Inject clocks and source clients into tests.
- Do not introduce a transformer stack or effect framework without a demonstrated need.
- Keep concurrency bounded and structured; cancellation and failures must propagate predictably.

### 6.4 Errors

Errors MUST be typed at subsystem boundaries and include context suitable for a user or diagnostic log. Do not catch every exception and turn it into a successful empty result.

Distinguish:
- Expected domain unavailability.
- Validation errors.
- Provider/network failures.
- Persistence failures.
- Programming defects.

The CLI may render a concise message while preserving structured diagnostic detail. A failed source must not silently yield a report that appears complete.

### 6.5 Determinism

Given the same versioned raw facts, normalisation rules, metric definitions, and configuration, the analytical output MUST be reproducible except for explicitly volatile metadata such as report-generation time.

Sort collections deterministically. Do not depend on network response order, hash-map iteration order, machine locale, or implicit timezone.

---

## 7. Haskell engineering standard

### 7.1 Compiler and project management

- Use a pinned GHC version and a reproducible build plan, via a maintained Cabal or Stack workflow.
- Commit the package manifest and the chosen lock/resolution artefact where applicable.
- Pin CI toolchain versions; upgrades occur through explicit change sets.
- Compile with `-Wall` and a reviewed set of additional warnings.
- Enable warnings for incomplete patterns and incomplete record updates.
- Treat project warnings as errors in CI after the initial warning baseline is clean.
- Keep compiler extensions minimal, documented, and local where practical.
- Do not add language extensions merely to shorten code.

The project MUST select one primary build workflow rather than maintain two competing workflows without a concrete need.

### 7.2 Type design

- Use algebraic data types to encode domain alternatives.
- Use `newtype` for semantically distinct primitive values.
- Prefer total constructors and smart constructors for validated values.
- Keep constructors private where exposing them would permit invalid states.
- Use phantom types or GADTs only when they materially enforce a domain invariant.
- Prefer ordinary, readable types over type-level machinery whose proof burden exceeds its benefit.
- Make units, periods, currencies, and consolidation scope explicit.
- Do not use `String` for every identifier and status.
- Do not create a bespoke abstraction when a standard type expresses the invariant clearly.

### 7.3 Functional design

- Pure core; effectful boundaries.
- Small functions with explicit inputs and outputs.
- Compose transformations rather than mutate intermediate state.
- Use folds and traversals where they express the algorithm clearly; do not use point-free style at the expense of proof and review.
- Use strictness deliberately for large data pipelines and long-lived accumulators.
- Benchmark before adding strictness annotations or lower-level optimisations.
- Avoid partial functions, unchecked parsing, broad exception swallowing, and silent default values.
- Prefer a small number of meaningful domain abstractions over generic frameworks.

### 7.4 Code style: terse, mathematical, reviewable

The code SHOULD be concise and algorithmic, but not cryptic. “Mathematically dense” means explicit invariants and compact definitions—not one-letter names, golfed expressions, or opaque cleverness.

Each core algorithm should have:
1. Preconditions.
2. Definition.
3. Failure/unavailability conditions.
4. Invariants or postconditions.
5. Tests that exercise the claims.

Example of a total checked ratio:

```haskell
data RatioError
  = ZeroDenominator
  | NonFiniteInput
  deriving (Eq, Show)

checkedRatio :: Double -> Double -> Either RatioError Double
checkedRatio numerator denominator
  | not (finite numerator && finite denominator) = Left NonFiniteInput
  | denominator == 0 = Left ZeroDenominator
  | otherwise = Right (numerator / denominator)
  where
    finite x = not (isNaN x || isInfinite x)
```

For financial calculations, a decimal or rational representation may be more suitable for particular operations. `Double` should be chosen deliberately, not by default. For valuation models, floating-point use is often reasonable, but NaN, infinity, sign, and sensitivity behaviour must be tested.

### 7.5 Documentation

Document public modules and non-obvious financial formulas. Prefer short, precise comments explaining why an invariant exists. Do not narrate every line.

Each metric definition should include a human-readable formula and interpretation note, alongside executable code and tests.

---

## 8. Coding standards — gold standard

### Mandatory rules

- No partial functions in production code.
- No `undefined`, `unsafePerformIO`, or unexplained `error`.
- No ignored compiler warnings in project-owned code.
- No untyped numeric constants where units or semantics matter.
- No silent fallback values for failed parsing or missing data.
- No broad catch-all exception handler that hides defects.
- No duplicate formula implementations across report renderers.
- No calculations embedded in string-rendering code.
- No live network dependency in unit tests.
- No unbounded retry loops or unbounded concurrency.
- No unchecked SQL string interpolation.
- No credentials or personal tokens in source control.
- No generated files, caches, or local databases committed unless explicitly designated as fixtures.
- No dependency added without purpose, licence review, maintenance review, and tests/build compatibility review.
- No code copied from a repository without confirming its licence and applicable notices.

### Review checklist

A change is ready to merge only when:
- Its domain and failure behaviour are explicit.
- It does not weaken existing invariants.
- Its tests cover normal, boundary, and invalid cases.
- It preserves provenance and reproducibility.
- It adds no unreviewed dependency or licence obligation.
- Formatting, linting, compilation, and relevant tests pass.
- User-visible output remains honest about uncertainty and data gaps.

### Naming and formatting

- Use descriptive names for domain concepts; short names are acceptable only inside tightly scoped mathematics where conventional.
- Keep functions focused on one transformation.
- Keep line length and formatting consistent through an automated formatter.
- Avoid manual alignment that creates noisy diffs.
- Prefer explicit units in names or types (`amount`, `perShare`, `periodEnd`) when ambiguity is plausible.
- Do not optimise for minimum line count. Optimise for minimum conceptual complexity.

---

## 9. Open-source package policy

Only well-maintained, battle-tested packages may be used in production dependencies. “Popular” or “has a Hackage page” is not sufficient evidence.

### 9.1 Admission checklist

For each direct dependency, record:
- Exact package name and version range.
- Purpose and modules used.
- Licence and compatibility with Seby's MIT licence.
- Upstream repository.
- Recent release/activity and maintainer responsiveness.
- Open issue/PR and security posture, where relevant.
- Test suite and CI status.
- Compatibility with the pinned GHC version.
- Transitive dependency footprint.
- Whether the standard library or a smaller dependency can do the job.
- Exit/replacement plan if the package becomes unmaintained.

Do not invent a universal “recent activity” cutoff. Evaluate maintenance relative to package maturity and risk. A stable package may not need frequent releases; a network client tied to a changing API does.

### 9.2 Preferred dependency categories

Choose the smallest reliable, battle-tested set for:
- CLI parsing: `optparse-applicative`.
- HTTP/TLS: `http-client`, `http-client-tls`.
- JSON and HTML/XML/XBRL parsing: `aeson`, `tagsoup` / `xeno` as needed.
- Financial arithmetic & currencies: `money` (`Data.Money` with type-level currencies, e.g. `Money "INR"`) and `Decimal` (`Data.Decimal` for fixed-point accounting line items without floating-point artifacts).
- SQLite access and migrations: `sqlite-simple` (or direct bindings) with explicit parameterised queries.
- Date/time handling: `time`.
- CSV parsing: `cassava`.
- Logging and structured diagnostics: `katip` or minimal structured logger.
- Property-based and example-based testing: `hedgehog` / `QuickCheck`, `hspec`.
- Formatting and linting: `fourmolu`, `cabal-fmt`, `hlint`.

Candidate libraries MUST be evaluated against current maintenance, licence, build, and API requirements at the time they are adopted. Do not treat this specification as a permanent endorsement of a package version.

### 9.3 Dependency controls

- Prefer direct, well-established packages over obscure wrappers around the same library.
- Avoid duplicate libraries for the same function without a documented reason.
- Use version bounds that prevent unreviewed breaking upgrades.
- Keep a dependency inventory and licence notices.
- Review security advisories and dependency changes periodically.
- Remove unused dependencies.
- Do not vendor a dependency unless required for reproducibility, patching, or a justified supply-chain constraint.
- A package failing maintenance review is a blocker for new adoption; existing use requires a migration decision rather than a silent upgrade/downgrade.

---

## 10. Financial theory and repository reuse policy

Financial logic MUST NOT rely on invented heuristics or ad-hoc formulas. Implementations MUST be anchored strictly in tried-and-tested open-source implementations where available, and canonical corporate finance literature (CFA Institute standards, Damodaran on Valuation, McKinsey Valuation) where libraries are absent.

### 10.1 Audit before reuse

Inspect:
1. Licence file and copyright notices.
2. Commit and release history.
3. Maintainer activity and issue/PR handling.
4. CI, tests, and supported toolchain.
5. Open defects and known security issues.
6. Dependencies and transitive dependencies.
7. Actual implementation and mathematical definitions.
8. Data-provider assumptions and endpoint age.
9. Build and test results in an isolated environment.
10. Whether the code fits Seby's data model and architecture.

Record the decision as **adopt**, **adapt**, **reference only**, or **reject**, with a short rationale.

### 10.2 Candidate repositories and theory references

Evaluations for candidate repositories and canonical theory references:

- **Financial amounts & currencies (`money` on Hackage / Kowainik):**
  - *Status:* **Adopt**.
  - *Rationale:* Gold standard for discrete currency representation with compile-time currency tags (`Money "INR"`), preventing cross-currency mixing and IEEE 754 precision loss.
- **Fixed-point statement numbers (`Decimal` / `Data.Decimal` on Hackage):**
  - *Status:* **Adopt**.
  - *Rationale:* Essential for Ind AS balance sheet and cash flow statements reported in Crores and Lakhs ($1\text{ Cr} = 10^7\text{ INR}$).
- `boundedvariation/quantfin` — quantitative-finance library:
  - *Status:* **Adapt / Reference only**.
  - *Rationale:* Excellent reference for day-count conventions (`Actual/365`, `30/360`), year fractions ($\Delta t$), and time-value-of-money discounting relative to a valuation anchor date $t_0$. Do not adopt heavy stochastic/derivatives modules.
- `MarcusRainbow/Ffinar` — pricing and term-structure interfaces:
  - *Status:* **Reference only**.
  - *Rationale:* Inspect date arithmetic and yield/discount curves for discounting mechanics.
- `McFrappe/HaskellQuant` — archived trading/backtesting project:
  - *Status:* **Reject / Reference only** (archived, unmaintained).
- `agrafix/yahoo-finance-conduit` & `cdepillabout/yahoo-finance-api`:
  - *Status:* **Reject** (APIs discontinued upstream; do not adopt).
- `jaredloomis/stock-analysis`:
  - *Status:* **Reference only** after verifying licence.
- `0xpantera/stockell`:
  - *Status:* **Reference only**.
- `lcleme9871/stocks-haskell`:
  - *Status:* **Reject** (GPL-3.0 licence is incompatible with Seby's MIT licence).

### 10.3 Canonical corporate finance standards (when library code is absent)

Where open-source Haskell packages focus primarily on derivatives pricing rather than fundamental corporate equity analysis, implement strictly according to:

1. **CFA Institute Financial Reporting and Analysis (FRA) Standards:**
   - **CAGR Preconditions:** CAGR is defined as $(V_n / V_0)^{1/n} - 1$ strictly when $V_0 > 0$ and $V_n > 0$. If either base or final value is non-positive, CAGR is mathematically undefined and economically not meaningful. Output must be tagged `Unavailable (NotMeaningful "...")`.
   - **Working Capital Ratios:** Days Sales Outstanding (DSO), Days Inventory Outstanding (DIO), and Days Payable Outstanding (DPO) MUST use average balance sheet numbers: $\frac{\text{Average Accounts Receivable}}{\text{Revenue}} \times 365$.
2. **Damodaran on Valuation (*Investment Valuation*, Wiley):**
   - **Discounting Timing:** Cash flow discounting in DCF uses elapsed duration / year fraction from the valuation anchor date ($\Delta t = t_i - t_0$), with mid-year convention ($\Delta t - 0.5$) for cash flows distributed across the financial year.
   - **Terminal Value:** Gordon Growth Model $\text{TV} = \frac{\text{CF}_{n+1}}{\text{WACC} - g}$ requires strict precondition $\text{WACC} > g$. If $\text{WACC} \le g$, return `Left (InvalidGrowthAssumption "Terminal growth exceeds cost of capital")`.
   - **EV to Equity Bridge:** $\text{Enterprise Value} = \text{Market Cap} + \text{Total Debt} + \text{Minority Interest} + \text{Preferred Stock} - \text{Cash and Equivalents}$.
3. **McKinsey & Co (*Valuation: Measuring and Managing the Value of Companies*):**
   - **ROIC Definition:** $\text{ROIC} = \frac{\text{NOPAT}}{\text{Invested Capital}}$, distinguishing operating assets from non-operating cash.
   - **Consolidated Scope:** Group fundamental assessment must strictly evaluate Consolidated accounts to prevent hiding debt in off-balance-sheet entities. Standalone is reserved for dividend upstream capacity.

---

## 11. MIT licence policy

Seby's own source code SHOULD be released under the MIT licence, unless the project owner chooses another licence before the first public release.

### Requirements

- Include the standard MIT `LICENSE` file with the copyright holder and year.
- Include a third-party notices file or equivalent inventory.
- Preserve required copyright and licence notices for dependencies and copied/adapted code.
- Confirm that every direct and transitive dependency has a licence compatible with the intended distribution.
- Do not assume that public code without a licence is available for reuse.
- Do not copy GPL-licensed code into the project merely because it is on GitHub. GPL obligations may be incompatible with the desired distribution model and must be reviewed before any incorporation.
- Do not remove original authorship or attribution notices.
- Record provenance for substantial adapted code.
- If licence status is ambiguous, do not incorporate the code until resolved.

MIT applies to Seby's code; it does not override third-party licences, provider terms, data copyrights, exchange terms, or website terms. Data redistribution and software licensing are separate questions.

---

## 12. Output formats and report contract

### 12.1 Required formats

**Release 1**
- Human-readable terminal output.
- Markdown report file.
- Machine-readable JSON for stable data exchange, once the report schema is defined and versioned.

**Later**
- CSV for tabular metrics and time series.
- HTML for navigable reports, if justified.
- Optional PDF only after a stable report model exists.

Output formats must render a shared report model. They MUST NOT implement separate financial calculations.

### 12.2 Output structure

A report SHOULD contain:
1. Company identity, ticker, exchange, and analysis timestamp.
2. Statement basis, currency, data coverage, and last-refresh information.
3. Summary of material findings with supporting metrics.
4. Five-year trends where available.
5. Profitability, growth, balance-sheet, cash-flow, and efficiency sections.
6. Valuation metrics and explicit model assumptions.
7. Peer and industry comparisons when comparability criteria pass.
8. Accounting and data-quality flags with evidence.
9. Corporate-action and comparability notes.
10. Missing-data register.
11. Metric definitions and provenance/source references.
12. Reproducibility metadata: Seby version, schema version, formula version, and source snapshot identifiers.

### 12.3 Output honesty

- Use explicit labels such as `Unavailable`, `Not applicable`, `Stale`, `Non-comparable`, or `Conflicting sources`.
- Do not turn unavailable values into zero, empty strings, or a green/red score.
- Display units and periods.
- Distinguish source-reported, normalised, derived, and estimated values.
- Do not rank a company on a metric that is not meaningful for its business model.
- Round only for presentation; retain suitable internal precision.
- JSON schema changes require versioning and tests.

### 12.4 Exit codes

Define and document stable exit-code classes, at minimum:
- Success.
- Invalid command or arguments.
- Unsupported/unknown entity.
- Incomplete analysis due to data availability.
- Source/network failure.
- Local storage or internal failure.

A partial but honest report may be emitted when some inputs are unavailable. Its status and warnings must be explicit.

---

## 13. Development phases and skeleton code

The skeletons below define boundaries and expected direction. They are illustrative compile-oriented starting points, not a complete implementation. They must be adapted to the selected package versions and module layout.

### Phase 0 — Repository and toolchain foundation

**Deliverables**
- Haskell package and pinned toolchain.
- MIT licence and third-party dependency policy.
- Formatting, linting, CI, test runner.
- CLI starts and prints version/help.
- No data providers yet.

**Acceptance**
- Clean checkout builds in CI.
- Warnings and formatting checks pass.
- Tests run offline.

Illustrative `app/Main.hs`:

```haskell
module Main (main) where

import Seby.CLI (runCLI)

main :: IO ()
main = runCLI
```

Illustrative `Seby/CLI.hs`:

```haskell
module Seby.CLI (runCLI) where

import Options.Applicative
import Seby.App (Command (..), runCommand)

runCLI :: IO ()
runCLI = execParser opts >>= runCommand
  where
    opts = info (commandParser <**> helper)
      (fullDesc <> progDesc "Analyse Indian listed equities")

commandParser :: Parser Command
commandParser =
  Analyse <$> subparser
    ( command "analyse"
        (info (AnalyseCompany <$> strArgument
          (metavar "SYMBOL" <> help "NSE/BSE symbol"))
          (progDesc "Analyse a listed company"))
    )
```

This CLI sketch may be simplified during implementation; the important requirement is a stable command model and no business logic inside argument parsing.

### Phase 1 — Pure domain and report from fixtures

**Deliverables**
- Typed identifiers, periods, units, quality states, facts.
- Pure formulas for first 15–20 metrics.
- Terminal and Markdown report from fixture data.
- Formula-level tests and golden reports.

Illustrative domain core:

```haskell
module Seby.Metrics
  ( MetricError (..)
  , growthRate
  , freeCashFlow
  ) where

import Seby.Domain.Types

data MetricError
  = NonPositiveGrowthBoundary  -- CFA standard: CAGR is undefined when base or ending value is <= 0
  | IncompatibleCurrencies
  | IncompatibleStatementBases
  | IncompatiblePeriods
  | ArithmeticOverflow
  deriving (Eq, Show)

-- | Total Compound Annual Growth Rate (CAGR) following CFA Institute standards.
-- Formula: (V_n / V_0)**(1/n) - 1.
-- Strictly defined only when both base and ending values are strictly positive.
growthRate :: PositiveYears -> Money -> Money -> Either MetricError Rate
growthRate years oldValue newValue
  | moneySign oldValue /= Positive || moneySign newValue /= Positive =
      Left NonPositiveGrowthBoundary
  | otherwise = do
      ratio <- moneyRatio newValue oldValue
      let n = yearsToDouble years
          r = (ratioToDouble ratio) ** (1.0 / n) - 1.0
      mkRate r

-- | Free Cash Flow = Operating Cash Flow - Capital Expenditure.
-- Enforces matching currency, period, and consolidation scope.
freeCashFlow :: Money -> Money -> Either MetricError Money
freeCashFlow operatingCashFlow capitalExpenditure =
  compatibleSubtract operatingCashFlow capitalExpenditure
```

The names above stand for validated domain operations. Do not implement `Money` as an unchecked numeric alias. Growth formulas must define how period length, sign, and currency are handled.

### Phase 2 — SQLite and provenance

**Deliverables**
- Versioned schema and migrations.
- Raw observation and provenance persistence.
- Atomic refresh transactions.
- Cache lookup and explicit refresh/offline modes.
- Reproducibility from stored facts.

Illustrative repository boundary:

```haskell
module Seby.Persistence.Repository
  ( FactRepository (..)
  ) where

data FactRepository m = FactRepository
  { storeRawObservation :: RawPayload -> [RawObservation] -> m (Either StoreError ())
  , storeCanonicalFacts :: [CanonicalFact] -> m (Either StoreError ())
  , loadCanonicalFacts  :: CompanyId -> AnalysisPeriod -> m (Either StoreError [CanonicalFact])
  }
```

The concrete SQLite adapter implements this interface. Financial functions receive canonical facts, never an open database connection.

### Phase 3 — First live source adapter

**Deliverables**
- One source adapter end-to-end.
- Timeouts, bounded retries, validation, and provenance.
- Recorded fixtures for offline integration tests.
- Source health diagnostics.
- Explicit stale-data behaviour.

Illustrative pipeline:

```haskell
data RefreshSummary = RefreshSummary
  { summaryCompanyId :: CompanyId
  , summaryPeriod    :: AnalysisPeriod
  , summaryFactCount :: Int
  , summaryTimestamp :: UTCTime
  } deriving (Eq, Show)

refreshCompany
  :: (Monad m)
  => SourceClient m
  -> FactRepository m
  -> CompanyId
  -> AnalysisPeriod
  -> m (Either RefreshError RefreshSummary)
refreshCompany source repository company period = do
  fetched <- fetchFacts source company period
  case fetched of
    Left err -> pure (Left (RefreshFetchError err))
    Right (payload, rawObs) -> do
      storedRaw <- storeRawObservation repository payload rawObs
      case storedRaw of
        Left err -> pure (Left (RefreshStoreError err))
        Right () -> case validateAndNormalise rawObs of
          Left err -> pure (Left (RefreshValidationError err))
          Right facts -> do
            saved <- storeCanonicalFacts repository facts
            case saved of
              Left err -> pure (Left (RefreshStoreError err))
              Right () -> pure (Right RefreshSummary
                { summaryCompanyId = company
                , summaryPeriod    = period
                , summaryFactCount = length facts
                , summaryTimestamp = payloadTimestamp payload
                })
```

Production code must retain raw observations and provenance, distinguish an empty valid response from a failed fetch, and avoid treating this simplified pipeline as sufficient for every source.

### Phase 4 — Analysis and peer comparison

**Deliverables**
- Peer-selection algorithm with reasons and exclusions.
- Industry medians where sample size and comparability are adequate.
- Quarterly/TTM calculations where reliable.
- Industry-specific applicability rules.

Illustrative pure ranking shape:

```haskell
import qualified Data.List.NonEmpty as NE

rankPeers
  :: CompanyProfile
  -> NonEmpty CompanyProfile
  -> NonEmpty PeerCandidate
rankPeers target candidates =
  NE.sortBy (comparing (Down . candidateScore)) candidatesWithScores
  where
    candidatesWithScores = NE.map toCandidate candidates
    toCandidate candidate =
      PeerCandidate
        { candidateProfile = candidate
        , candidateScore   = peerScore target candidate
        }
```

`peerScore` must be documented and tested; the final system must explain the selected peers and must not imply statistical certainty from a heuristic score.

### Phase 5 — Valuation and sensitivity

**Deliverables**
- DCF with explicit inputs and assumptions.
- Bear/base/bull cases.
- Sensitivity analysis and reverse DCF where inputs permit.
- Unit and property tests for numerical edge cases.

Illustrative DCF signature:

```haskell
-- | Discounted cash flows using elapsed year fractions from the valuation anchor date.
-- In accordance with corporate finance theory (Damodaran), discount periods represent
-- elapsed duration (\Delta t = t_i - t_0), not raw calendar years, supporting optional
-- mid-year discounting (\Delta t - 0.5).
discountedCashFlows
  :: DiscountRate
  -> NonEmpty (YearFraction, Money)
  -> Either ValuationError Money
discountedCashFlows discount flows = do
  discounted <- traverse (uncurry (discountCashFlow discount)) flows
  sumMoney discounted
```

The production model must validate discount rates, year ordering, currency, terminal-value assumptions, and non-finite intermediate results. This skeleton omits those details intentionally.

### Phase 6 — Corporate actions, segments, and forensic signals

**Deliverables**
- Dated corporate-action ledger.
- Reported, comparable/restated, and point-in-time views.
- Segment-aware analysis where disclosures support it.
- Evidence-based flags and neutral language.
- Regression fixtures for known difficult cases.

No event should be applied to historical data without an explicit rule, source, effective date, and test.

### Phase 7 — Reliability, scale, and user workflow

**Deliverables**
- Batch analysis and watchlists.
- Cache and database maintenance commands.
- Stress and performance benchmarks.
- Offline reproducibility and failure-recovery tests.
- Optional HTML/PDF and portfolio analytics only when justified.

Do not start Phase 7 before the core calculations, provenance, and source reconciliation are trustworthy.

---

## 14. Rigorous testing strategy

Testing must establish properties of the calculation system, not merely show that a command exits with status zero.

### 14.1 Test layers

1. **Unit tests:** formulas, parsing, date handling, normalisation, status transitions.
2. **Property tests:** mathematical invariants over generated inputs.
3. **Golden tests:** hand-verified source fixtures and report snapshots.
4. **Integration tests:** adapter-to-database and database-to-report workflows.
5. **Contract tests:** source payload schemas and expected provider behaviours.
6. **Migration tests:** old database to current schema, including data preservation.
7. **Fault-injection tests:** timeouts, malformed payloads, rate limits, partial responses, disk errors.
8. **Stress tests:** large histories, many companies, cache growth, repeated refreshes, bounded concurrency.
9. **Numerical tests:** extreme magnitudes, near-zero denominators, NaN/infinity boundaries, overflow/underflow where applicable.
10. **Regression tests:** every fixed defect gets a minimal reproducing test.

### 14.2 Mathematical properties

Examples to encode as property tests, subject to domain preconditions:

- For positive denominator \(d\), checked division satisfies \(q \times d \approx n\), within a defined floating-point tolerance.
- Discount factors are positive for supported discount-rate domains.
- Increasing a positive discount rate cannot increase the present value of positive future cash flows, all else equal.
- A zero cash flow contributes zero present value.
- Reordering independent input observations does not change a deterministic aggregate.
- Re-running the same analysis over the same immutable inputs and formula version yields the same derived metrics.
- A failed refresh leaves the previous committed dataset intact.
- A missing value never becomes numeric zero through parsing, normalisation, storage, or rendering.
- Per-share series spanning a split or bonus event must either be consistently adjusted or explicitly marked non-comparable.
- Unit conversions round-trip within a defined tolerance where exact arithmetic is not possible.

Each property must define its valid domain. Do not assert a mathematical property outside the model's assumptions.

### 14.3 Outlier and adversarial inputs

Test at least:
- Zero, negative, and extremely small denominators.
- Negative earnings and negative equity.
- Zero or negative revenue.
- Extremely high leverage and interest expense.
- Missing quarters and irregular fiscal years.
- Duplicate filings and revised results.
- Unit errors (rupees vs crores, shares vs lakhs of shares).
- Currency mismatches.
- Consolidated/standalone mixing attempts.
- Invalid dates, timezone offsets, leap days, and period overlaps.
- Extremely large/small numbers, NaN, positive/negative infinity.
- Sudden share-count changes and corporate actions.
- Out-of-order observations and duplicate source payloads.
- Malformed, truncated, unexpectedly empty, and schema-shifted provider responses.
- HTML pages returned where JSON was expected, including rate-limit or anti-bot pages.
- SQL interruption, locked database, disk-full and failed migration conditions.
- A source returning plausible but conflicting values.
- A source changing symbol mappings or removing a previously available field.

Outliers must not be silently clipped or discarded. Apply robust statistics only when the method is documented, the raw value is retained, and the report explains the treatment.

### 14.4 Stress and performance tests

Benchmarks should cover:
- Cold-cache single-company analysis.
- Warm-cache single-company analysis.
- Repeated refreshes with unchanged data.
- Batch runs across hundreds or thousands of securities using synthetic or lawfully collected fixtures.
- Large filing histories and large raw payloads.
- Database size and query latency as history grows.
- Maximum configured source concurrency.
- Recovery after cancellation or a provider outage.

Record baseline runtime, memory, request count, database size, and error rate. Define performance targets after measuring the first correct implementation. Do not make up performance claims before benchmarking.

### 14.5 Test data policy

- Prefer deterministic, versioned fixtures.
- Keep fixture provenance and licensing clear.
- Avoid making routine CI depend on live NSE/BSE/Yahoo endpoints.
- Use live smoke tests only as a separate optional workflow with strict rate limits.
- Never commit secrets or personal data in fixtures.
- Use synthetic fixtures for rare, extreme, and adversarial cases.

---

## 15. CI specification (`.github/workflows/ci.yml`)

CI MUST run on pull requests and pushes to the primary branch. It MUST use a pinned Haskell toolchain and cache dependencies without allowing a cache hit to replace validation.

Required jobs/checks:
1. Checkout and toolchain setup.
2. Dependency resolution/build-plan verification.
3. Formatting check.
4. Lint/static checks.
5. Compile with warnings enabled and warnings treated as errors for project code.
6. Unit and property tests.
7. Golden and offline integration tests.
8. Test coverage report or trend artefact.
9. Dependency/licence inventory check.
10. Optional build of executable artefact.

Live provider tests SHOULD be separate, manually triggered or scheduled, and MUST NOT gate ordinary pull requests unless the test environment is controlled.

### Example workflow

This is a baseline template. Pin action versions to reviewed immutable commit SHAs in the production repository; update them through explicit maintenance changes. Replace tool versions only through a tested toolchain upgrade.

```yaml
name: CI

on:
  push:
    branches: [main]
  pull_request:
  workflow_dispatch:

permissions:
  contents: read

concurrency:
  group: ci-${{ github.workflow }}-${{ github.ref }}
  cancel-in-progress: true

jobs:
  build-test:
    name: Build and test
    runs-on: ubuntu-latest
    timeout-minutes: 20

    steps:
      - name: Checkout
        uses: actions/checkout@v4

      - name: Set up GHC and Cabal
        uses: haskell-actions/setup@v2
        with:
          ghc-version: 'PINNED_GHC_VERSION'
          cabal-version: 'PINNED_CABAL_VERSION'

      - name: Cache Cabal packages
        uses: actions/cache@v4
        with:
          path: |
            ~/.cabal/store
            dist-newstyle
          key: ${{ runner.os }}-ghc-PINNED_GHC_VERSION-${{ hashFiles('**/*.cabal', 'cabal.project', 'cabal.project.freeze') }}
          restore-keys: |
            ${{ runner.os }}-ghc-PINNED_GHC_VERSION-

      - name: Update package index
        run: cabal update

      - name: Check formatting (Fourmolu & cabal-fmt)
        run: |
          fourmolu --mode check $(git ls-files '*.hs')
          cabal-fmt --check $(git ls-files '*.cabal')

      - name: Build with warnings as errors
        run: cabal build all --enable-tests --ghc-options="-Wall -Werror"

      - name: Run tests
        run: cabal test all --test-show-details=direct

      - name: Check dependency plan
        run: cabal build all --dry-run

      - name: Upload test logs on failure
        if: failure()
        uses: actions/upload-artifact@v4
        with:
          name: ci-logs
          path: |
            dist-newstyle
          if-no-files-found: ignore
```

### CI implementation notes

- The `PINNED_*` placeholders MUST be replaced with real, supported versions before merging; the template is not ready to run unchanged.
- Pin every third-party GitHub Action to a reviewed full commit SHA in the production workflow, rather than relying indefinitely on movable tags.
- Code formatting MUST be verified using `fourmolu` for Haskell sources and `cabal-fmt` for Cabal project files.
- Add the project's property-test, golden-test, coverage, and licence-check commands once selected.
- Ensure `-Werror` policy is applied to project code without creating unmaintainable noise from third-party package builds.
- Do not cache build artefacts across incompatible compiler or dependency plans.
- The workflow must be tested in GitHub Actions before being considered complete.
- A green CI run is necessary, not sufficient: reviewers remain responsible for domain correctness and licence compliance.

---

## 16. Observability and diagnostics

- Logs SHOULD be structured and concise.
- Each refresh run should have a correlation/run ID.
- Record source, elapsed time, retry count, status, and row counts.
- Never log credentials, cookies, authentication headers, or sensitive local paths unnecessarily.
- User-facing errors should state what failed and whether a partial report was produced.
- Diagnostic mode may provide detailed source and calculation traces without cluttering normal output.

---

## 17. Security and operational requirements

- Store no secrets in the repository.
- Do not bypass source access controls, rate limits, or anti-bot mechanisms.
- Validate downloaded content before parsing.
- Set payload-size and parser-complexity limits to reduce denial-of-service risk.
- Use parameterised SQL.
- Treat HTML, JSON, CSV, and XBRL content as untrusted input.
- Keep dependencies patched through reviewed updates.
- Make destructive database operations explicit and require confirmation where appropriate.
- Network failure, malformed input, or an unsupported company must not cause silent data corruption.

---

## 18. Definition of done

A feature is complete only when all applicable conditions hold:

- Domain meaning and assumptions are documented.
- Input and output types represent invalid or missing states explicitly.
- No partial function is introduced into production paths.
- Tests cover normal, boundary, invalid, outlier, and regression cases.
- Provenance and formula/version metadata are preserved.
- Offline reproducibility is maintained.
- Data-source and licence constraints are respected.
- Formatting, linting, compilation, and CI pass.
- Output communicates uncertainty and data gaps.
- The feature is small enough to review and has a clear rollback or migration path.

## 19. Implementation priorities

1. Toolchain, licence, CI, formatting, and module boundaries.
2. Domain types, data-quality states, units, and periods.
3. Pure formulas and hand-verified fixtures.
4. Shared report model and terminal/Markdown output.
5. SQLite schema, provenance, migrations, and cache.
6. One primary source adapter and an approved fallback.
7. End-to-end analysis for a small representative company set.
8. Peer/industry analysis.
9. Valuation and sensitivity.
10. Corporate actions, segment analysis, and forensic flags.
11. Stress hardening and broader batch operation.
12. Additional output formats and portfolio features.

Do not implement all phases before using the tool. Each phase must leave a coherent, testable repository.

---

## 20. Final engineering rule

**Every number must have a lineage. Every formula must have a definition. Every failure must have a representation. Every dependency must justify its existence. Every release must be reproducible.**

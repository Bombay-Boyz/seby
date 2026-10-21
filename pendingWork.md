# Pending Work to Complete Seby

This document is a phase-wise handoff plan for completing the product described
in [`seby-project-vision.md`](seby-project-vision.md) and
[`seby-engineering-spec.md`](seby-engineering-spec.md). It is intended to let a
new contributor start a phase without having to reconstruct product decisions
from chat history.

## Current baseline

The repository has completed the initial engineering phases through **Phase 8
(portfolio snapshots and reporting)**. The current implementation includes a
typed Haskell domain, SQLite fact storage and migrations, sourced statement
adapters, offline/cache workflows, terminal and Markdown company reports,
peer-ranking primitives, DCF calculations, corporate-action/segment/forensic
analysis primitives, watchlists and database maintenance, a synthetic
performance benchmark, and an offline portfolio-snapshot command.

The foundation is useful, but the broader project vision is not complete. In
particular, the company workflow still depends on explicit local imports or
provider choices; the available provider coverage and reporting basis vary;
peer and valuation calculations are not yet a complete sourced company workflow;
and later research, portfolio, and resilience features remain limited.

The phases below continue the phase numbering in the engineering spec. They
are a proposed completion sequence, **not authorization to begin any phase**.
Work should start one phase at a time after explicit approval.

## Completion target

Use two completion gates so “project complete” has a concrete meaning:

1. **First useful release:** a user can run `seby analyse SYMBOL` for a small,
   documented set of supported listed companies; Seby resolves the security,
   retrieves or reuses data, produces a transparent report, and can reproduce
   that report offline from the saved inputs.
2. **Full vision:** comparative analysis, valuation, corporate-structure and
   forensic coverage, and the research/portfolio workflows in Releases 2–4 of
   `seby-project-vision.md` are implemented to the scoped acceptance criteria
   below.

Do not expand to all NSE/BSE listings before the first gate works reliably for
the supported set. A small, well-supported universe is preferable to a broad
unverified one.

---

## Phase 9 — Supported issuer resolution and end-to-end analysis

**Outcome:** `seby analyse SYMBOL` works for a declared, small initial universe
without requiring the user to know provider-specific ticker syntax.

### Tasks

- Define and publish the initial supported-company set and its coverage limits.
- Add stable company and security records, including exchange and provider
  symbol mappings with effective dates. A ticker must not be used as the
  permanent company identity.
- Add deterministic symbol resolution and a useful typed error for unknown or
  ambiguous symbols.
- Define the source-selection policy for each data class: identity, financial
  statements, and market prices. Confirm source terms, endpoint coverage, rate
  limits, and standalone/consolidated semantics before relying on an adapter.
- Add a single analysis use case that resolves the entity, applies cache
  freshness policy, refreshes when permitted, persists validated data, and
  builds the report.
- Preserve the existing explicit `refresh`, `prefer-cache`, and `offline`
  controls. Rendering and metric calculations must remain network-free.
- Make an incomplete or assumed reporting basis visible in the report and
  explain its source.

### Gold-standard solution

Use a normalized security master backed by effective-dated identifier
mappings. Keep provider adapters replaceable and inject their interfaces into
an orchestration layer. Validate and reconcile each fetched batch before the
atomic repository write. A failed refresh must leave the last valid snapshot
usable and must record the failed attempt. The default command should follow
the same policy as the explicit refresh command, with a documented default
cache age and no hidden network behavior in pure analysis code.

Maintain a checked-in, versioned fixture for every supported provider
contract. Treat provider success as transport success only: schema, units,
dates, currency, basis, and coverage still need validation.

### Suggested interfaces

```haskell
data SecurityMapping = SecurityMapping
  { mappingCompany :: !CompanyId
  , mappingSecurity :: !SecurityId
  , mappingExchange :: !Exchange
  , mappingLocalSymbol :: !Symbol
  , mappingProvider :: !SourceId
  , mappingProviderSymbol :: !Symbol
  , mappingEffectiveFrom :: !Day
  , mappingEffectiveUntil :: !(Maybe Day)
  }

data EntityResolutionError
  = UnknownSymbol !Symbol
  | AmbiguousSymbol !Symbol ![SecurityMapping]
  | NoMappingForSource !SecurityId !SourceId

data AnalysisRequest = AnalysisRequest
  { requestSymbol :: !Symbol
  , requestBasis :: !StatementBasis
  , requestScope :: !Text
  , requestPeriod :: !DateRange
  , requestCachePolicy :: !CacheMode
  }

data AnalysisEnvironment = AnalysisEnvironment
  { resolveSecurity :: Symbol -> IO (Either EntityResolutionError SecurityMapping)
  , refreshFacts :: SecurityMapping -> CacheMode -> IO (Either RefreshError RefreshSummary)
  , loadFacts :: CompanyId -> DateRange -> IO (Either StoreError [CanonicalFact])
  }

runCompanyAnalysis
  :: AnalysisEnvironment
  -> AnalysisRequest
  -> IO (Either AnalysisError Report)
```

The type names are proposed. Reuse the current repository interfaces where they
fit; do not add empty abstractions just to mirror this sketch.

### Acceptance criteria

- A supported symbol resolves to the same stable company/security identity
  regardless of provider symbol format.
- Unknown and ambiguous symbols produce actionable errors and nonzero exit
  status.
- An offline run makes zero provider requests and reproduces the same report
  from the same saved facts and policy versions.
- Cache reuse, explicit refresh, fallback, and all-source failure each have
  fixture-driven tests.
- The report shows company identity, coverage, basis, periods, freshness, and
  any source assumptions.

---

## Phase 10 — Financial statement coverage and trend analysis

**Outcome:** the first supported companies have enough validated history for a
useful fundamental report, rather than a narrow set of income and cash-flow
facts.

### Tasks

- Extend source mappings and canonical facts for balance-sheet concepts,
  shares/EPS where reliable, and the working-capital fields needed by selected
  ratios.
- Target five annual periods where the source provides them; show fewer periods
  honestly where it does not.
- Add quarterly facts and calculate TTM only from a complete, compatible set of
  quarters. Do not mix standalone and consolidated data or incompatible fiscal
  periods.
- Define and version units, sign conventions, normalisation rules, and formula
  versions. Keep raw source values unchanged.
- Build coverage metadata per concept, period, basis, and source. Distinguish
  missing, not disclosed, not retrieved, not applicable, non-comparable, and
  calculation failure.
- Expand toward the Release 1 metric set: growth, EPS/share count, margins,
  ROE/ROCE, leverage and liquidity where meaningful, cash conversion, and
  valuation inputs where source data is sufficient.
- Add formula definitions and report notes for every derived metric.

### Gold-standard solution

Keep ingestion mapping separate from calculations. Convert provider fields into
typed canonical facts with unit and basis metadata; calculate metrics only from
compatible canonical inputs. Centralize period alignment and TTM validation.
Every derived metric should retain its input fact identities, formula version,
and any assumptions. Report builders should return explicit availability or
quality states instead of omitting a value without explanation.

### Suggested interfaces

```haskell
data FactCoverage = FactCoverage
  { coverageConcept :: !Text
  , coveragePeriods :: ![FactPeriod]
  , coverageSource :: !(Maybe SourceId)
  , coverageStatus :: !CoverageStatus
  , coverageExplanation :: !Text
  }

data DerivedMetric = DerivedMetric
  { derivedName :: !Text
  , derivedValue :: !(Observed Rate)
  , derivedInputs :: !(NonEmpty FactReference)
  , derivedFormulaVersion :: !Text
  }

deriveTtm
  :: MetricDefinition
  -> [CanonicalFact]
  -> Either MetricDerivationError (Observed Money)

buildTrendSeries
  :: MetricDefinition
  -> [CanonicalFact]
  -> Either MetricDerivationError [DerivedMetric]
```

Adapt `Observed`, `DataIssue`, and the existing GADT metric model rather than
creating a parallel quality system.

### Acceptance criteria

- Golden fixtures cover at least one supported company with five annual years
  and a separate fixture with incomplete history.
- TTM rejects missing quarters, overlaps, wrong fiscal sequences, and basis or
  currency mismatches.
- Formula tests cover negative values, zero denominators, unit conversion,
  missing periods, and restatements.
- Reports show the available history and explicit gaps; they never turn missing
  data into zero.
- Re-running derivation from unchanged facts and formula versions is
  deterministic.

---

## Phase 11 — Peer and industry analysis in the product workflow

**Outcome:** peer ranking and industry summaries are based on versioned,
comparable company data and appear in user-facing reports.

### Tasks

- Define an initial, versioned industry taxonomy and company classification
  source. Record who/what assigned classifications and when they are valid.
- Build peer profiles from stored facts and market capitalization measured at a
  common date; do not require users to construct `CompanyProfile` values in
  code.
- Integrate existing `Seby.Analysis.Peers` ranking into a `seby peers SYMBOL`
  command and the company report.
- Show eligible peers, exclusions, reasons, data completeness, and profile
  dates. Allow a manual peer override without hiding the automatic result.
- Calculate industry medians only for comparable observations and a documented
  minimum sample. Report sample size and the dispersion/coverage limitations.
- Add segment-level comparison only when segment disclosures and peer scopes
  are compatible.

### Gold-standard solution

Use explicit eligibility before scoring: same industry group, period, basis,
currency, and an adequate set of comparable facts. Keep the existing heuristic
score a ranking aid, never a probability. Preserve all exclusions and reasons.
Use deterministic tie breaking, version the classification and score policy,
and make small-sample results unavailable instead of presenting weak medians as
industry truth.

### Suggested interfaces

```haskell
data PeerDataset = PeerDataset
  { peerDatasetAsOf :: !Day
  , peerDatasetVersion :: !Text
  , peerProfiles :: ![CompanyProfile]
  }

data PeerReport = PeerReport
  { peerTarget :: !CompanyProfile
  , peerSelection :: !PeerSelection
  , industryContext :: !(Either IndustrySummaryError IndustrySummary)
  }

loadComparablePeerDataset
  :: FactRepository
  -> CompanyId
  -> Day
  -> IO (Either PeerDataError PeerDataset)

buildPeerReport
  :: PeerDataset
  -> CompanyId
  -> Either PeerDataError PeerReport
```

### Acceptance criteria

- Fixture tests demonstrate exact peers, excluded peers, ties, missing data, and
  insufficient industry samples.
- The report explains why peers were chosen and why candidates were excluded.
- All peer inputs use the same reporting period, basis, currency, and market
  capitalization date, or are explicitly excluded.
- Industry medians expose the sample count and do not run below the configured
  minimum.
- Results reproduce from the saved facts plus the taxonomy/ranking versions.

---

## Phase 12 — Integrated valuation and market context

**Outcome:** valuation is a sourced, reproducible part of company analysis while
keeping analyst assumptions separate from reported facts.

### Tasks

- Add a validated as-of market price and share-count path for supported
  securities. Calculate market capitalization only when share count and price
  are compatible and dated appropriately.
- Build valuation inputs from explicit historical facts and clearly labeled
  analyst forecasts; do not infer a forecast silently.
- Add a documented enterprise-to-equity bridge (cash, debt, minority interest,
  diluted shares) when reliable inputs exist. Keep unavailable bridge items
  unavailable; do not substitute zero.
- Integrate the existing DCF bear/base/bull, sensitivity, and reverse-DCF
  functions into a CLI/report workflow.
- Add supported market multiples (P/E, P/B, EV/EBITDA or EV/FCF) with
  industry-specific applicability and explicit numerator/denominator dates.
- Add historical valuation ranges only after comparable historical prices,
  actions, and fundamentals are available.

### Gold-standard solution

Model a valuation as a dated input manifest: source facts, analyst assumptions,
formula version, timing convention, and output components. Keep enterprise value
separate from equity value and per-share value. Every scenario must surface its
discount rate, terminal growth, forecast cash flows, and sensitivity grid.
Reject stale or mismatched prices unless a visible policy explicitly permits
them. Avoid false precision in rendered value ranges.

### Suggested interfaces

```haskell
data EquityBridgeInputs = EquityBridgeInputs
  { bridgeCash :: !(Observed Money)
  , bridgeDebt :: !(Observed Money)
  , bridgeMinorityInterest :: !(Observed Money)
  , bridgeDilutedShares :: !(Observed ShareCount)
  }

data CompanyValuationRequest = CompanyValuationRequest
  { valuationAsOf :: !Day
  , valuationForecasts :: !(NonEmpty ProjectedCashFlow)
  , valuationAssumptions :: !DcfAssumptions
  , valuationBridge :: !(Maybe EquityBridgeInputs)
  }

buildCompanyValuation
  :: CompanyFacts
  -> CompanyValuationRequest
  -> Either CompanyValuationError ValuationReport
```

This is a shape, not an instruction to reuse an `Observed` constructor that
does not match its actual GADT parameters. Introduce a domain type only after
defining its units and missing-data semantics.

### Acceptance criteria

- Golden valuation fixtures use hand-checked cash flows and assumptions.
- Sensitivity monotonicity and numerical boundaries are tested.
- Reports label all analyst inputs separately from sourced observations.
- Enterprise, equity, and per-share values are not conflated.
- Unavailable/stale prices or bridge components produce explicit status and
  warnings.
- Reverse DCF is described as implied market expectations, not a forecast.

---

## Phase 13 — Complete corporate-structure and forensic coverage

**Outcome:** historical comparisons reflect disclosed changes without silently
rewriting reported data, and investigation flags remain evidence-based.

### Tasks

- Extend the action ledger for mergers, acquisitions, demergers/spin-offs,
  rights issues, buybacks, new issuance/dilution, ticker/name changes, and
  material changes in consolidation scope.
- Add effective, publication, and ingestion timestamps where needed to support
  both comparable history and point-in-time views.
- Add persisted provenance and correction history for actions and segment
  disclosures.
- Build segment-level metrics and peer comparison where disclosures permit;
  report reconciliation difference and unallocated amounts.
- Expand forensic rules to supported inputs such as receivables growth versus
  revenue, inventory, unusual capitalization, restatements, auditor notes, and
  cash conversion.
- Add normalization policy for cyclical earnings only with explicit inputs and
  documented sector assumptions.
- Surface material evidence and benign explanations in reports; never present
  a statistical flag as proof of misconduct.

### Gold-standard solution

Store source observations immutably and make each adjustment a separate,
versioned view. Represent event dates and knowledge dates independently.
Adjustment functions should be pure, return the applied-event list and
comparability issues, and fail closed when an event is ambiguous. Each forensic
rule should be a small typed function returning the observed evidence, formula,
period, provenance references, severity rationale, and possible benign
explanations.

### Suggested interfaces

```haskell
data TemporalView
  = AsReported
  | ComparableThrough !Day
  | KnownAt !UTCTime

data EvidenceFlag = EvidenceFlag
  { flagRuleId :: !Text
  , flagRuleVersion :: !Text
  , flagObservation :: !Text
  , flagEvidence :: !(NonEmpty FactReference)
  , flagPeriod :: !FactPeriod
  , flagLimitations :: ![Text]
  , flagBenignExplanations :: ![Text]
  }

buildTemporalCompanyView
  :: TemporalView
  -> CompanyId
  -> FactRepository
  -> IO (Either TemporalViewError CompanyFacts)
```

### Acceptance criteria

- Every adjustment is backed by an event source, rule version, and regression
  fixture.
- As-reported facts remain unchanged after adjustment.
- Point-in-time queries exclude information not published by the requested
  date.
- Segment totals reconcile to reported totals or show a precise difference.
- Every forensic signal identifies its evidence and limitations and avoids
  accusatory language.

---

## Phase 14 — Research and portfolio workflow

**Outcome:** extend the completed local portfolio snapshot into repeatable
research workflows, only after the company analysis and price history are
dependable.

### Tasks

- Decide which research workflow has demonstrated user value before adding
  features. Candidate work includes saved portfolios, price refreshes,
  concentration/exposure, total returns, drawdown, volatility/correlation,
  filing-change detection, thesis/change ledger, point-in-time backtests, and
  scheduled local reports.
- If saving portfolios, add a versioned schema and migrations for portfolio,
  position/lot, transaction, price observation, and snapshot records. Preserve
  historical holdings and source lineage.
- Add price adjustments and total-return treatment only after the relevant
  corporate-action and dividend policies are implemented.
- Keep the existing offline JSON portfolio path as a deterministic import and
  reproduction route.
- Consider HTML/PDF only if a real use case justifies the extra renderer and
  layout QA.

### Gold-standard solution

Represent positions as dated transactions/lots rather than overwriting one
quantity and average cost. Make valuation dates and source prices explicit.
Calculate concentration and risk statistics from a documented return series,
not from unrelated snapshots. For mixed currencies, either report separate
currency totals (the present behavior) or require a cited, dated FX source and
show its impact separately. Backtests must use point-in-time facts and prices
that were actually available at each historical date.

### Suggested interfaces

```haskell
data PortfolioTransaction
  = Buy !SecurityId !Quantity !Money !UTCTime !Provenance
  | Sell !SecurityId !Quantity !Money !UTCTime !Provenance
  | CashDividend !SecurityId !Money !UTCTime !Provenance

data PortfolioSnapshotRequest = PortfolioSnapshotRequest
  { snapshotPortfolioId :: !PortfolioId
  , snapshotAsOf :: !Day
  , snapshotCurrencyPolicy :: !CurrencyPolicy
  }

buildPortfolioSnapshot
  :: PortfolioRepository
  -> PriceRepository
  -> PortfolioSnapshotRequest
  -> IO (Either PortfolioAnalysisError PortfolioReport)
```

### Acceptance criteria

- Transaction replay and portfolio snapshots reproduce deterministically.
- Buy/sell, dividends, splits, missing prices, and FX treatment have tests and
  explicit policy.
- Drawdown, volatility, and correlation state their return frequency, date
  range, benchmark, and missing-observation treatment.
- Portfolio imports remain offline and digest-linked.
- Any database schema change has migration and failure-recovery tests.

---

## Phase 15 — Production hardening and release readiness

**Outcome:** the project can make bounded claims about reliability, performance,
coverage, and reproducibility for its declared supported universe.

### Tasks

- Extend Phase 7 benchmarks to measure process memory, request count, error
  rate, cold/warm analysis, database growth/query latency, configured source
  concurrency, cancellation, and provider-outage recovery.
- Add controlled local-server tests for timeout, `Retry-After`, retry bounds,
  malformed/schema-shifted payloads, rate limiting, partial data, and fallback
  disagreement.
- Add persistence fault tests for locked database, interrupted transaction,
  failed migration, and write failure. Disk-full tests may use a controlled
  environment if reliable simulation is available.
- Enforce input and parser limits consistently for all importers. Review URL,
  symbol, numeric, and date validation and ensure error messages do not expose
  secrets.
- Review migrations, backups/restores, dependency licences, data-source terms,
  and secret handling.
- Run CI on the pinned toolchain and treat its result as a release gate. The
  existing workflow covers formatting, lint, warning-as-error build, tests,
  coverage artifact, and dependency-plan checks; the spec also calls for a
  dependency/licence inventory and a verified workflow run.
- Resolve the outstanding `cabal check` warning for the benchmark's `-O2`
  option by either measuring/justifying it or removing it and refreshing the
  recorded baseline.
- Produce a support matrix listing company, exchange/security mapping, source,
  basis, history coverage, last verified date, and known limitations.

### Gold-standard solution

Keep ordinary CI offline and deterministic. Use a local fake server or injected
source client for network failure tests; never make routine CI depend on live
provider availability. Record machine/toolchain and exact workload for
benchmarks, and publish limitations rather than extrapolating a single run.
Generate a release checklist from the supported-company matrix and require the
full clean-checkout workflow before tagging a release.

### Suggested interfaces

```haskell
data SourceAttempt = SourceAttempt
  { attemptSource :: !SourceId
  , attemptNumber :: !Int
  , attemptStartedAt :: !UTCTime
  , attemptFinishedAt :: !UTCTime
  , attemptOutcome :: !AttemptOutcome
  }

data SourcePolicy = SourcePolicy
  { policyTimeout :: !NominalDiffTime
  , policyMaximumAttempts :: !Int
  , policyMaximumConcurrency :: !Int
  , policyRetryAfter :: !Bool
  }

runSourceRequest
  :: SourcePolicy
  -> SourceClient
  -> Request
  -> IO (Either SourceError (Response, [SourceAttempt]))
```

The implementation should use existing HTTP and exception facilities rather
than introducing a new effect framework without evidence that it is needed.

### Acceptance criteria

- Clean checkout completes format, lint, warning-as-error build, tests, and
  benchmark build in CI.
- Offline tests cover representative success, boundary, invalid, and recovery
  paths across source, persistence, and report layers.
- The performance report includes all available measures and explicitly labels
  those not measured.
- Backups restore into a valid database and preserve committed WAL data.
- The support matrix and data-source/licence review are current for the release.
- No unsupported coverage, freshness, or performance claim appears in user
  output or release notes.

---

## Suggested phase order and release gates

1. **Phase 9:** end-to-end workflow for a small supported company set. This is
   the first useful-release blocker.
2. **Phase 10:** improve financial history and metrics for that set; complete
   the first useful release only when offline reproduction and coverage
   reporting are sound.
3. **Phases 11–12:** comparative analysis and integrated valuation (Release 2).
4. **Phase 13:** corporate structure and forensic depth (Release 3).
5. **Phase 14:** research and advanced portfolio workflows (Release 4).
6. **Phase 15:** maintain a continuous hardening lane; complete it before each
   release rather than deferring all reliability work to the end.

Each phase should have a separate implementation brief, fixture plan, and
acceptance checklist before coding begins. Do not start the following phase
until the current phase passes its acceptance criteria and the user approves
the transition.

## Current known limits to keep visible

- Statement adapters require explicit source/provider symbols and API
  credentials; free-tier coverage for Indian listings is not guaranteed.
- The default company identity workflow is not yet a complete exchange security
  master and does not resolve arbitrary NSE/BSE listings.
- Portfolio analysis currently consumes a supplied snapshot; it does not fetch
  market prices, persist transactions, apply FX, or calculate total returns.
- Peer selection and DCF are available as calculation components but need
  broader sourced inputs and user-facing integration described above.
- The recorded benchmark does not measure memory, live provider latency/error
  rate, cancellation, or outage recovery latency.
- Terminal and Markdown are supported; HTML/PDF are optional and not release
  blockers unless a concrete user need justifies them.


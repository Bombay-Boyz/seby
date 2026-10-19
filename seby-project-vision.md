# Seby — Project Vision

**Project name:** Seby  
**Executable:** `seby`  
**Project type:** Local-first, command-line equity research and fundamental-analysis tool  
**Initial market:** Indian listed equities (NSE and BSE)  
**Primary implementation language:** Haskell  
**Operating principle:** Evidence before conclusions; reproducibility before polish; useful scope before feature count.

---

## 1. Vision

Seby aims to be a rigorous, transparent, and fast fundamental-analysis system for Indian listed companies. A user should be able to run:

```sh
seby analyse TCS
```

…and receive a useful, structured research report without manually selecting ratios, supplying peers, or assembling data from multiple websites.

The long-term goal is not to produce a pile of ratios. It is to build a dependable analytical system that connects reported financial data, business economics, accounting quality, capital allocation, valuation, and uncertainty—while showing exactly where every conclusion came from.

Seby should become a tool whose calculations can be inspected, challenged, reproduced, and improved. It should favour sound analysis over confident-sounding output.

## 2. Product principles

1. **Evidence-linked conclusions.** Every material metric and analytical claim should trace to source data, a formula, a period, and any relevant adjustment.
2. **No fabricated certainty.** Missing, stale, incomparable, estimated, and not-applicable values must be distinguished from genuinely poor performance.
3. **Local-first and cost-conscious.** Prefer local storage and free, legally usable data sources. Avoid mandatory paid APIs or recurring infrastructure costs.
4. **One-command workflow.** The default path should be simple; complexity belongs in the engine, not in the user's routine.
5. **Haskell-first.** Build the core in Haskell because it is the preferred language. Do not switch languages solely on the assumption that a rewrite will save time.
6. **Reuse responsibly.** Prefer mature, maintained libraries for HTTP, parsing, SQLite, CLI handling, dates, and numerical primitives. Reuse specialist code only after checking its fit, build status, tests, and licence.
7. **Financially meaningful comparisons.** Compare like with like: consolidated with consolidated, compatible accounting periods, relevant peers, and industry-appropriate metrics.
8. **Explicit uncertainty.** Valuations should expose assumptions and sensitivities rather than imply false precision.
9. **Incremental delivery.** The first release must be useful on its own. Advanced features should be earned by actual use.
10. **No unsupported accusations.** Forensic signals are prompts for investigation, not proof of fraud or misconduct.

## 3. Intended users and use cases

Seby is initially a personal research tool for analysing Indian listed companies. Its output should help answer:

- Is the business profitable, and are returns on capital attractive?
- Is growth translating into cash?
- Is the balance sheet resilient?
- Are earnings supported by operating cash flow?
- How efficiently does the company use working capital and assets?
- How does the business compare with relevant peers and its broader industry?
- What expectations are embedded in the current share price?
- Which assumptions most affect estimated value?
- What changed in the company's financial position or business structure?
- Which data gaps or accounting signals require further investigation?

Seby is an analytical aid, not an automated buy/sell oracle or a substitute for reading important filings.

## 4. Product experience

### Default command

```sh
seby analyse TCS
```

The command should resolve the listed entity, select appropriate financial statements, retrieve or reuse cached data, calculate supported metrics, identify peers, and generate a report.

### Useful future commands

The following are illustrative and should be added only when needed:

```sh
seby analyse TCS --period 5y
seby analyse TCS --standalone
seby peers TCS
seby filings TCS
seby history TCS
seby watchlist add TCS
seby data-check TCS
```

The default report should work without flags. Options should expose control, not make ordinary use complicated.

### Report format

The initial output should be readable in a terminal and exportable to Markdown. A future HTML report may be useful, but should not block the first release.

A report should include:
- A concise company and data-coverage summary.
- Business-quality and profitability measures.
- Growth and five-year trends.
- Balance-sheet and liquidity assessment.
- Cash-flow and earnings-quality analysis.
- Valuation measures with assumptions.
- Peer and industry context where data is comparable.
- Data-quality warnings and corporate-action notes.
- Source references, reporting periods, and calculation definitions.
- A short list of questions worth investigating next.

## 5. Scope by release

The scope is deliberately staged. The vision is broad; the first usable release must remain small.

### Release 1 — Useful, trustworthy baseline

**Goal:** Make `seby analyse TCS` genuinely useful for a first group of companies.

- Haskell CLI with a predictable command structure.
- Company/symbol resolution for supported NSE/BSE listings.
- Local SQLite cache and a clear refresh policy.
- A source adapter for market prices and available financial statements.
- Annual financial history, targeting five years when available.
- Approximately 15–20 high-value metrics, prioritising:
  - Revenue, operating profit, net profit, and EPS growth.
  - ROE, ROCE, and margins where inputs support reliable calculation.
  - Debt/equity and interest coverage where meaningful.
  - Operating cash flow, free cash flow, and cash conversion.
  - P/E, P/B, EV/EBITDA or other valuation measures when inputs are available.
- Basic trend analysis and transparent calculation notes.
- Markdown report output.
- Explicit missing-data and comparability warnings.
- Tests for financial formulas and data normalisation.

**Release 1 is complete when:** a user can analyse a supported company end-to-end, inspect the sources and formulas, understand the important limitations, and reproduce the result from cached inputs.

### Release 2 — Comparative analysis

- Automatic selection of three relevant peers, with manual override.
- Broader industry medians, not just a three-company peer set.
- Peer comparisons for growth, profitability, returns on capital, leverage, cash conversion, and valuation.
- Quarterly and trailing-twelve-month metrics where reliable data is available.
- ROIC versus cost of capital and economic-profit analysis, with explicit assumptions.
- More robust cash-flow and working-capital analysis.
- DCF with bear/base/bull cases and sensitivity tables.
- Historical valuation ranges where sufficient consistent history exists.

### Release 3 — Corporate structure and forensic depth

- Segment-aware analysis for diversified businesses and conglomerates.
- Segment-level peer selection when disclosures permit it.
- Corporate-action ledger covering mergers, acquisitions, demergers, splits, bonus issues, rights issues, dilution, ticker/name changes, and material changes in consolidation.
- Clear distinction between reported history, comparable/restated history, and point-in-time information for backtesting.
- Accounting and earnings-quality flags, including cash conversion divergence, receivables growing faster than revenue, unusual capitalisation, restatements, and auditor qualifications.
- Normalised earnings for cyclical businesses.
- Reverse DCF to show what expectations the market price implies.
- More extensive scenario analysis.

### Release 4 — Research and portfolio workflow

Only after the analysis engine is dependable:

- Watchlists and company monitoring.
- Filing-change detection and a thesis/change ledger.
- Point-in-time historical snapshots for backtesting.
- Portfolio exposure, concentration, drawdown, volatility, and correlation analysis.
- Optional evidence-grounded narrative summaries.
- Batch analysis and scheduled local reports.

Monte Carlo simulation and more advanced quantitative methods are optional extensions, not prerequisites for useful fundamental analysis.

## 6. Analytical framework

The long-term metric catalogue may include the following families. Not every metric applies to every company, and the report must explain exclusions.

### A. Business quality and profitability

- Gross, operating, EBITDA, and net margins where definitions are consistent.
- ROE, ROA, ROCE, and ROIC.
- Incremental returns on capital.
- DuPont decomposition.
- Profitability trend and stability.

### B. Growth

- Three-year and five-year revenue, EPS, net profit, EBITDA, operating cash flow, and free-cash-flow growth.
- Book value per share and dividend-per-share growth where meaningful.
- Growth quality: whether profit growth is accompanied by cash generation and sensible capital needs.

CAGRs must not be calculated across incompatible or zero/negative base values without an explicit alternative treatment.

### C. Balance-sheet resilience

- Debt/equity, net debt/EBITDA, debt/assets, and interest coverage.
- Current, quick, and cash ratios where useful.
- Debt-service measures when the required data is available.
- Maturity, liquidity, and refinancing context if reliable disclosures are available.

### D. Cash flow and earnings quality

- Operating cash flow relative to net income.
- Free cash flow and FCF margin.
- Capital expenditure relative to revenue.
- Accrual and cash-conversion measures.
- Receivables growth compared with revenue growth.
- Working-capital movement and its effect on cash generation.

### E. Operating efficiency

- Asset, inventory, and receivables turnover.
- Days sales outstanding, days inventory outstanding, and days payable outstanding.
- Cash-conversion cycle.
- Use average balance-sheet values when the calculation calls for them and both periods are available.

### F. Valuation

- P/E, P/B, P/S, EV/EBITDA, EV/EBIT, EV/FCF, earnings yield, and FCF yield when meaningful.
- PEG only where growth inputs make it interpretable.
- DCF, reverse DCF, and bear/base/bull scenarios.
- Historical valuation percentiles when the historical data is consistent.
- Margin of safety relative to a range of estimated values.

Forward valuation ratios must not be presented as facts unless credible forward estimates exist. DCF output must display its assumptions and sensitivity to key variables.

### G. Shareholder returns and dilution

- Dividend yield and payout ratio.
- Buyback and other capital-return context when data supports it.
- Share-count growth and dilution.
- EPS growth versus net-profit growth.
- Total shareholder return, adjusted for relevant corporate actions when reliable data is available.

### H. Industry-specific models

Generic industrial-company ratios must not be blindly applied to banks, insurers, NBFCs, utilities, REITs, or other structurally different businesses. Seby should select an appropriate model or explicitly mark a metric as not meaningful. Industry-specific coverage can expand incrementally.

## 7. Data strategy

### Preferred source hierarchy

1. **Primary disclosures:** NSE/BSE filings, company annual reports, quarterly results, investor presentations, and other official disclosures.
2. **Structured public data:** official machine-readable or XBRL data when available and usable.
3. **Secondary data sources:** free providers for prices, reference data, or convenience, subject to availability and terms.
4. **Manual research aids:** tools such as financial-screening websites may help with cross-checking, but should not be treated as a stable API unless an authorised interface exists.

Free access does not automatically mean permission for unrestricted automated collection. Every adapter must respect source terms, rate limits, and technical restrictions. Unofficial endpoints can break; they must be isolated behind replaceable adapters.

### Data model requirements

Every material observation should retain, where available:
- Company identifier and security/symbol.
- Metric or raw statement item.
- Value, unit, currency, and sign convention.
- Period start/end and period type.
- Filing/publication date and retrieval timestamp.
- Source URL or stable source identifier.
- Standalone or consolidated basis.
- Reported, restated, adjusted, estimated, or derived status.
- Formula/version for derived metrics.
- Quality, completeness, and comparability flags.

### Source and data failure policy

- Cache data locally in SQLite.
- Avoid silently replacing a valid observation with a failed or partial fetch.
- Show when data was last updated.
- Distinguish “not disclosed,” “not retrieved,” “not applicable,” “not comparable,” “not meaningful” (e.g. CAGR across non-positive values), and “calculation failed.”
- Preserve raw source values separately from normalised values.
- Make source adapters replaceable without rewriting the analysis engine.
- Never fabricate missing values merely to complete a table.

## 8. Peer and industry selection

Peer selection should consider:
- Industry and business model.
- Revenue sources and segment mix.
- Company size and market capitalisation.
- Geography and regulatory context where relevant.
- Capital intensity and financial structure.
- Data completeness and reporting comparability.

The three closest peers are a useful compact comparison, not a substitute for industry-wide context. Seby should show why peers were selected and flag weak matches. For conglomerates, a single peer set may be misleading; segment-level comparisons should be used only when disclosures support them.

## 9. Corporate actions and comparability

Corporate actions can make naive historical comparisons wrong. Seby should maintain a corporate-action ledger and avoid silently rewriting history.

For material events, distinguish:
1. **As reported:** figures exactly as published for the relevant period.
2. **Comparable/restated:** values adjusted or restated for a clearly documented reason.
3. **Point-in-time:** what was publicly known by a given historical date, for future backtesting.

Relevant events include mergers, acquisitions, demergers, spin-offs, share splits, bonus issues, rights issues, buybacks, new share issuance, ticker/name changes, and changes in consolidation scope.

If a reliable adjustment cannot be made, mark the series as non-comparable and explain why. Do not infer undisclosed adjustments.

## 10. Valuation philosophy

Valuation is a range of conditional outcomes, not a single objective truth.

Seby should:
- Separate observed market data from analyst assumptions.
- Display the model, inputs, units, and calculation date.
- Use bear/base/bull scenarios where the data supports them.
- Expose sensitivity to growth, margins, reinvestment, discount rate, terminal growth, and dilution.
- Avoid unsupported forward estimates.
- Treat reverse DCF as an exploration of implied expectations, not proof of what will happen.
- Avoid spurious precision in fair-value estimates.
- Use industry-appropriate methods where a generic DCF or ratio is unsuitable.

Cost of equity, WACC, terminal growth, and normalised earnings require explicit assumptions and should not be disguised as directly observed facts.

## 11. Accounting and forensic signals

Seby may identify signals that warrant further investigation, such as:
- Persistent divergence between reported profits and operating cash flow.
- Receivables or inventory growing materially faster than sales.
- Unusual capitalisation or abrupt changes in accounting estimates.
- Repeated restatements or auditor qualifications.
- Large changes in related-party balances, contingent liabilities, or working capital, where reliable disclosures are available.
- Material share dilution or changes in consolidation scope.

These are **investigation flags**, not verdicts. A flag must state the observation, calculation, time period, data source, and plausible benign explanations where relevant. Seby must not label a company fraudulent based only on a ratio or anomaly.

## 12. Technical architecture

A suitable high-level architecture is:

```text
CLI
 └── Use cases / report orchestration
      ├── Entity and security resolution
      ├── Source adapters
      │    ├── Official filings / structured disclosures
      │    ├── Market-price provider(s)
      │    └── Optional secondary providers
      ├── Raw-data store and provenance
      ├── Normalisation and comparability
      ├── Financial calculation engine
      ├── Peer / industry analysis
      ├── Valuation and scenario engine
      └── Report renderers (terminal, Markdown; HTML later)

SQLite
 ├── Entities and securities
 ├── Raw observations and statements
 ├── Filings and source references
 ├── Corporate actions
 ├── Derived metrics and formula versions
 └── Cache metadata
```

Recommended design boundaries:
- Keep network access out of the core calculation engine.
- Use typed domain models for periods, currencies, statement basis, units, and data-quality states.
- Separate raw facts from derived metrics.
- Make calculations pure where practical and test them independently.
- Version formulas and normalisation rules.
- Treat data ingestion as fallible and analysis as explicit about missing inputs.
- Keep report generation separate from financial calculations.
- Prefer a small set of dependable dependencies over a large collection of poorly maintained packages.

This is a logical architecture, not a mandate to build every module before the first release.

## 13. Quality standards

Seby should aim for reference-quality calculations, not merely a large feature list.

### Testing

- Unit tests for every financial formula.
- Golden tests using hand-verified example statements.
- Property tests for invariants where useful.
- Regression tests for previously identified data and calculation errors.
- Tests for negative earnings, zero denominators, missing periods, currency/unit mismatches, restatements, and corporate actions.
- Integration tests for source adapters, with fixtures so tests do not depend on live websites.
- Reproducible report fixtures for a small, carefully chosen set of companies.

### Reviewability

- Document each metric's formula and definition.
- State whether inputs use average or period-end balances.
- Document treatment of exceptional items and negative values.
- Explain differences from source-provider ratios.
- Keep data-quality warnings visible in exported reports.
- Record enough provenance to reproduce a historical calculation.

### Performance and resilience

- Use local caching to avoid repeated network calls.
- Keep source adapters isolated so one provider failure does not break all analysis.
- Provide clear errors for unsupported symbols, incomplete data, rate limits, and network failures.
- Optimise only after profiling identifies a meaningful bottleneck.

## 14. Risks and constraints

| Risk | Mitigation |
|---|---|
| Free data sources change or restrict access | Use replaceable adapters, cache raw data, prefer official disclosures |
| Financial statements differ across providers | Preserve provenance, reconcile against primary filings, flag disagreements |
| Peer selection is misleading | Explain selection and show broader industry context |
| Corporate actions corrupt growth or valuation history | Maintain event ledger and distinguish reported from comparable series |
| Generic metrics mislead for financial companies or conglomerates | Use industry-specific models and segment-aware analysis |
| DCF creates false confidence | Show assumptions, scenarios, sensitivity, and limitations |
| Scope grows beyond available time | Protect the Release 1 boundary and ship incrementally |
| Reusing code introduces legal or maintenance problems | Verify licence, dependencies, activity, tests, and compatibility before adoption |
| A forensic flag is misread as an accusation | Use neutral language and show evidence and alternative explanations |

## 15. Explicit non-goals

At least for the initial releases, Seby is not intended to be:
- A broker, trading terminal, or order-execution system.
- A guaranteed stock-picking or buy/sell recommendation engine.
- A paid-data-dependent service.
- A replacement for reading material company filings.
- A system that invents forecasts to fill missing data.
- A generic derivatives-pricing platform.
- A web service requiring a permanently running server.
- A multi-language project merely to use different tools for different modules.

## 16. Definition of success

### Release 1 success
- `seby analyse TCS` produces a coherent report for supported companies.
- Key metrics agree with independently checked calculations.
- Every material figure has period and source context.
- Missing and non-comparable data are explicit.
- The report is useful without a subscription or cloud service.
- A failed data provider does not corrupt stored data or crash unrelated calculations.

### Long-term success
- The analysis is explainable and reproducible.
- Historical comparisons respect accounting and corporate-action changes.
- Peer and industry context is defensible.
- Valuation makes uncertainty visible.
- The tool improves research quality rather than merely increasing the volume of output.
- New features are added because they solve demonstrated problems, not because the architecture anticipated them.

## 17. Recommended implementation order

1. Define domain types and the data-quality vocabulary.
2. Build a working CLI and a Markdown report from a small fixture dataset.
3. Implement and test the first 15–20 metrics using hand-verified inputs.
4. Add SQLite schema, raw-data persistence, provenance, and caching.
5. Implement one reliable source adapter end-to-end.
6. Validate the full path on a small set of companies with different business models.
7. Add broader source coverage and quarterly data.
8. Add peers and industry medians.
9. Add valuation scenarios and sensitivity analysis.
10. Add corporate-action and segment-aware handling.
11. Add monitoring, backtesting, and portfolio features only when the core is dependable.

The first milestone is not “all Indian equities.” It is one complete, transparent, tested analysis path that can be expanded without redesigning the whole system.

## 18. Guiding rule

**Build the smallest version that is worth using, keep every conclusion traceable to evidence, and let real use determine what deserves to be built next.**

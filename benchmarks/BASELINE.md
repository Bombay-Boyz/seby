# Synthetic performance baseline

Recorded 2026-10-21 with:

- GHC 9.14.1 and Cabal 3.16.1.0
- Intel Core i5-8250U, x86_64 Linux
- Command: `cabal bench seby-baseline --enable-benchmarks --offline`

| Workload | Input | Iterations | Mean wall time |
| --- | ---: | ---: | ---: |
| Single company report | 600 facts, 100 annual periods | 50 | 0.279 ms/report |
| Batch report build | 200 companies, 24,000 facts total | 3 | 121.193 ms/batch (0.606 ms/report) |
| Identical SQLite refreshes | 600 facts, 10 refreshes/iteration | 3 | 295.468 ms/iteration |
| Large raw payload write | 4 MiB payload | 1 | 67.662 ms |
| First-connection SQLite analysis | 600 facts, 100 annual periods | 1 | 30.656 ms |
| Warm SQLite analyses | 600 facts, 100 annual periods | 100/iteration | 3,404.035 ms/iteration (34.040 ms/analysis) |

The benchmark builds synthetic facts before timing and fully renders reports.
SQLite measurements use a temporary local database. The resulting database was
5,001,216 bytes after the workload; the WAL was checkpointed to zero bytes when
measured. Provider requests were zero and the configured maximum source
concurrency was one (sequential). These are initial local reference values, not
performance targets. This run does not measure process memory, concurrent
provider load, OS-cold cache latency, or outage/cancellation recovery latency.
Repeat on the same hardware and use multiple runs before drawing conclusions.

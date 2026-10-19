# Third-party dependency inventory

This inventory covers direct dependencies declared by `seby.cabal`. Exact
resolved versions and the transitive dependency set are recorded in
`cabal.project.freeze`. Dependencies are linked, not copied into Seby; their
own licence and copyright notices continue to apply when redistributed.

| Package | Bound | Purpose | Licence | Upstream |
| --- | --- | --- | --- | --- |
| `base` | `>= 4.18 && < 5` | Haskell runtime and standard library | BSD-3-Clause | [GHC](https://www.haskell.org/ghc/) |
| `text` | `>= 2.0 && < 2.2` | Text values and rendering | BSD-2-Clause | [haskell/text](https://github.com/haskell/text) |
| `bytestring` | `>= 0.11 && < 0.13` | Binary source payloads | BSD-3-Clause | [haskell/bytestring](https://github.com/haskell/bytestring) |
| `containers` | `>= 0.6 && < 0.9` | Ordered collection types | BSD-3-Clause | [haskell/containers](https://github.com/haskell/containers) |
| `time` | `>= 1.12 && < 1.16` | Calendar and timestamp types | BSD-2-Clause | [haskell/time](https://github.com/haskell/time) |
| `optparse-applicative` | `>= 0.17 && < 0.20` | Typed command-line parsing | BSD-3-Clause | [pcapriotti/optparse-applicative](https://github.com/pcapriotti/optparse-applicative) |
| `QuickCheck` | `>= 2.14 && < 2.20` | Generated property testing | BSD-3-Clause | [nick8325/quickcheck](https://github.com/nick8325/quickcheck) |
| `crypton` | `>= 1.0.4 && < 2.2` | SHA-256 payload integrity (`Crypto.Hash`) | BSD-3-Clause, MIT, ISC | [kazu-yamamoto/crypton](https://github.com/kazu-yamamoto/crypton) |
| `sqlite-simple` | `>= 0.4.19 && < 0.5` | Parameterised SQLite persistence | BSD-3-Clause | [nurpax/sqlite-simple](https://github.com/nurpax/sqlite-simple) |
| `directory` (test only) | `>= 1.3 && < 1.4` | Temporary database lifecycle in integration tests | BSD-3-Clause | [haskell/directory](https://github.com/haskell/directory) |

## Phase 2 dependency review

| Package | Bound | Upstream | Review |
| --- | --- | --- | --- |
| `crypton` | `>= 1.0.4 && < 2.2` | [kazu-yamamoto/crypton](https://github.com/kazu-yamamoto/crypton) | Maintained cryptographic primitives; used only for standard SHA-256 content digests. Hackage declares BSD-3-Clause, MIT, and ISC licensing. |
| `sqlite-simple` | `>= 0.4.19 && < 0.5` | [nurpax/sqlite-simple](https://github.com/nurpax/sqlite-simple) | Stable mid-level, parameterised SQLite API; project builds locally with the pinned GHC 9.14.1. Hackage declares BSD3. |
| `directory` (test only) | `>= 1.3 && < 1.4` | [haskell/directory](https://github.com/haskell/directory) | Standard filesystem support for isolated temporary-database tests. Hackage declares BSD-3-Clause. |

The freeze file pins the complete resolved dependency graph for reproducible
builds. Review licence metadata, maintenance, compiler compatibility, and
security status again before adding or upgrading dependencies. Dependencies
are linked, not vendored; their upstream licence and copyright terms continue
to apply when Seby is redistributed.

# Third-party dependency inventory

This inventory covers direct dependencies declared by `seby.cabal`. Exact
resolved versions and the transitive dependency set are recorded in
`cabal.project.freeze`. Dependencies are linked, not copied into Seby; their
own licence and copyright notices continue to apply when redistributed.

| Package | Purpose | Licence |
| --- | --- | --- |
| `base` | Haskell runtime and standard library | BSD-3-Clause |
| `text` | Text values and rendering | BSD-2-Clause |
| `bytestring` | Binary data support for future ingestion boundaries | BSD-3-Clause |
| `containers` | Standard ordered collection types | BSD-3-Clause |
| `time` | Calendar and timestamp types | BSD-2-Clause |
| `optparse-applicative` | Typed command-line parsing | BSD-3-Clause |
| `QuickCheck` | Generated property testing | BSD-3-Clause |

The freeze file pins the resolved dependency graph for reproducible builds.
Review licence metadata, maintenance, compiler compatibility, and security
status again before adding or upgrading dependencies.

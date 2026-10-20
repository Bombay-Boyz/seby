module Main (main) where

import Seby.AlphaVantageSpec qualified as AlphaVantageSpec
import Seby.DomainSpec qualified as DomainSpec
import Seby.MetricsSpec qualified as MetricsSpec
import Seby.PeersSpec qualified as PeersSpec
import Seby.PersistenceSpec qualified as PersistenceSpec
import Seby.Phase6Spec qualified as Phase6Spec
import Seby.ReportingSpec qualified as ReportingSpec
import Seby.ValuationSpec qualified as ValuationSpec

main :: IO ()
main = do
  DomainSpec.runTests
  AlphaVantageSpec.runTests
  PeersSpec.runTests
  ValuationSpec.runTests
  Phase6Spec.runTests
  MetricsSpec.runTests
  ReportingSpec.runTests
  PersistenceSpec.runTests
  putStrLn "\n========================================="
  putStrLn
    "All Seby domain, ingestion, peer, valuation, corporate-action, segment, forensic, reporting, and persistence tests passed cleanly!"
  putStrLn "========================================="

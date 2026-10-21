module Main (main) where

import Seby.AlphaVantageSpec qualified as AlphaVantageSpec
import Seby.DomainSpec qualified as DomainSpec
import Seby.FmpSpec qualified as FmpSpec
import Seby.MetricsSpec qualified as MetricsSpec
import Seby.PeersSpec qualified as PeersSpec
import Seby.PersistenceSpec qualified as PersistenceSpec
import Seby.Phase6Spec qualified as Phase6Spec
import Seby.Phase7Spec qualified as Phase7Spec
import Seby.PortfolioSpec qualified as PortfolioSpec
import Seby.ReportingSpec qualified as ReportingSpec
import Seby.ValuationSpec qualified as ValuationSpec

main :: IO ()
main = do
  DomainSpec.runTests
  AlphaVantageSpec.runTests
  PeersSpec.runTests
  PortfolioSpec.runTests
  ValuationSpec.runTests
  Phase6Spec.runTests
  Phase7Spec.runTests
  FmpSpec.runTests
  MetricsSpec.runTests
  ReportingSpec.runTests
  PersistenceSpec.runTests
  putStrLn "\n========================================="
  putStrLn
    "All Seby domain, ingestion, peer, valuation, corporate-action, portfolio, segment, forensic, reporting, and persistence tests passed cleanly!"
  putStrLn "========================================="

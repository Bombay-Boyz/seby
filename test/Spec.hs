module Main (main) where

import Seby.DomainSpec qualified as DomainSpec
import Seby.AlphaVantageSpec qualified as AlphaVantageSpec
import Seby.MetricsSpec qualified as MetricsSpec
import Seby.PeersSpec qualified as PeersSpec
import Seby.PersistenceSpec qualified as PersistenceSpec
import Seby.ReportingSpec qualified as ReportingSpec

main :: IO ()
main = do
  DomainSpec.runTests
  AlphaVantageSpec.runTests
  PeersSpec.runTests
  MetricsSpec.runTests
  ReportingSpec.runTests
  PersistenceSpec.runTests
  putStrLn "\n========================================="
  putStrLn "All Seby domain, reporting, and persistence tests passed cleanly!"
  putStrLn "========================================="

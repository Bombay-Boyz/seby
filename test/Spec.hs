module Main (main) where

import Seby.DomainSpec qualified as DomainSpec
import Seby.MetricsSpec qualified as MetricsSpec
import Seby.PersistenceSpec qualified as PersistenceSpec
import Seby.ReportingSpec qualified as ReportingSpec

main :: IO ()
main = do
  DomainSpec.runTests
  MetricsSpec.runTests
  ReportingSpec.runTests
  PersistenceSpec.runTests
  putStrLn "\n========================================="
  putStrLn "All Seby domain, reporting, and persistence tests passed cleanly!"
  putStrLn "========================================="

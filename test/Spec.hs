module Main (main) where

import Seby.DomainSpec qualified as DomainSpec
import Seby.MetricsSpec qualified as MetricsSpec
import Seby.ReportingSpec qualified as ReportingSpec

main :: IO ()
main = do
  DomainSpec.runTests
  MetricsSpec.runTests
  ReportingSpec.runTests
  putStrLn "\n========================================="
  putStrLn "All Seby Phase 1 tests passed cleanly!"
  putStrLn "========================================="

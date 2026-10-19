{-# LANGUAGE OverloadedStrings #-}

module Seby.ReportingSpec (
  runTests,
) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text qualified as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Seby.Domain.Money (fromPaise, fromRupees)
import Seby.Domain.Period (FiscalPeriod (..), FiscalQuarter (..), mkDateRange, mkFiscalYear)
import Seby.Domain.Types (
  CompanyId (..),
  DataIssue (..),
  Days (..),
  Metric (..),
  Provenance (..),
  SourceId (..),
  StatementBasis (..),
  Symbol (..),
  mkMultiple,
  mkRate,
 )
import Seby.Reporting.Markdown (renderMarkdown)
import Seby.Reporting.Model (MetricObservation (..), mkCompanyIdentity, mkReport)
import Seby.Reporting.Terminal (renderTerminal)
import System.Exit (exitFailure)

assertEq :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEq label expected actual =
  if expected == actual
    then putStrLn $ "  [PASS] " ++ label
    else do
      putStrLn $
        "  [FAIL] " ++ label ++ "\n    Expected: " ++ show expected ++ "\n    Actual:   " ++ show actual
      exitFailure

runTests :: IO ()
runTests = do
  putStrLn "Running Seby.Reporting tests..."
  identity <- case mkCompanyIdentity (CompanyId "company-1") (Symbol "TCS") "Tata|Consultancy" of
    Left err -> do
      putStrLn $ "  [FAIL] valid company identity rejected: " ++ show err
      exitFailure
    Right value -> pure value
  assertEq
    "company identity rejects blank display names"
    True
    ( case mkCompanyIdentity (CompanyId "company-1") (Symbol "TCS") "  \t " of
        Left _ -> True
        Right _ -> False
    )

  fiscalYear2024 <- expectRight "construct report fiscal year" (mkFiscalYear 2024)
  quarterMargin <- expectRight "construct report margin" (mkRate 0.25)
  peMultiple <- expectRight "construct report multiple" (mkMultiple 10)
  customPeriod <-
    expectRight
      "construct report date range"
      (mkDateRange (fromGregorian 2024 1 1) (fromGregorian 2024 3 31))

  let source =
        Provenance
          { provenanceSource = SourceId "NSE"
          , provenanceUri = Just "https://example.test/results"
          , provenanceRetrievedAt = UTCTime (fromGregorian 2024 5 1) 0
          , provenancePublishedAt = Just (UTCTime (fromGregorian 2024 4 30) (12 * 60 * 60))
          , provenanceDigest = "sha256:fixture"
          }
      revenue = MetricObservation Revenue (fromRupees 100) (AnnualPeriod fiscalYear2024) (source :| [])
      grossMarginRow = MetricObservation GrossMargin quarterMargin (QuarterlyPeriod fiscalYear2024 Q2) (source :| [])
      peRow = MetricObservation PriceToEarnings peMultiple (CustomPeriod customPeriod) (source :| [])
      negativeProfit = MetricObservation NetProfit (fromPaise (-1)) (AnnualPeriod fiscalYear2024) (source :| [])
      dsoRow = MetricObservation DaysSalesOut (Days 36) (AnnualPeriod fiscalYear2024) (source :| [])
      report =
        mkReport
          identity
          (fromGregorian 2024 6 1)
          Consolidated
          [revenue, grossMarginRow, peRow, negativeProfit, dsoRow]
          [NotRetrieved]

  assertEq
    "terminal output includes identity, period, units, and source"
    ( Text.unlines
        [ "Seby Analysis Report"
        , "Company: Tata|Consultancy (TCS)"
        , "Analysis date: 2024-06-01"
        , "Statement basis: Consolidated"
        , ""
        , "Metrics"
        , "  Revenue: ₹100.00 INR (FY 2024; source: NSE — https://example.test/results (retrieved 2024-05-01 00:00:00 UTC; published 2024-04-30 12:00:00 UTC; digest sha256:fixture))"
        , "  Gross margin: 25.0% (Q2 FY 2024; source: NSE — https://example.test/results (retrieved 2024-05-01 00:00:00 UTC; published 2024-04-30 12:00:00 UTC; digest sha256:fixture))"
        , "  Price to earnings: 10.0x (2024-01-01 to 2024-03-31; source: NSE — https://example.test/results (retrieved 2024-05-01 00:00:00 UTC; published 2024-04-30 12:00:00 UTC; digest sha256:fixture))"
        , "  Net profit: -₹0.01 INR (FY 2024; source: NSE — https://example.test/results (retrieved 2024-05-01 00:00:00 UTC; published 2024-04-30 12:00:00 UTC; digest sha256:fixture))"
        , "  Days sales outstanding: 36 days (FY 2024; source: NSE — https://example.test/results (retrieved 2024-05-01 00:00:00 UTC; published 2024-04-30 12:00:00 UTC; digest sha256:fixture))"
        , ""
        , "Data quality"
        , "  - Not retrieved"
        ]
    )
    (renderTerminal report)

  let markdown = renderMarkdown report
  assertEq
    "Markdown escapes table delimiters in untrusted identity fields"
    True
    ("Tata\\|Consultancy" `Text.isInfixOf` markdown)
  assertEq
    "Markdown includes provenance and coverage context"
    True
    ("NSE — https://example.test/results" `Text.isInfixOf` markdown)
  assertEq
    "Markdown labels unavailable values without converting them to numbers"
    True
    ("Not retrieved" `Text.isInfixOf` markdown)
  putStrLn "Seby.Reporting tests passed successfully."

expectRight :: (Show errorValue) => String -> Either errorValue value -> IO value
expectRight label result = case result of
  Left err -> do
    putStrLn $ "  [FAIL] " ++ label ++ "\n    Unexpected error: " ++ show err
    exitFailure
  Right value -> pure value

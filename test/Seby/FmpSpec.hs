{-# LANGUAGE OverloadedStrings #-}

module Seby.FmpSpec (runTests) where

import Data.ByteString.Char8 qualified as ByteString
import Data.List (find)
import Data.Text qualified as Text
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Seby.Domain.Types (
  CompanyId (..),
  PeriodType (..),
  Provenance (..),
  SourceId (..),
  Symbol (..),
  provenanceSource,
 )
import Seby.Ingestion.FundamentalProviders (
  decodeAlphaVantageStatements,
  decodeFmpStatements,
  fetchAlphaVantageStatements,
  fetchFmpStatements,
 )
import Seby.Persistence.Types (
  canonicalAmountMinor,
  canonicalConcept,
  canonicalPeriodStart,
  canonicalPeriodType,
  canonicalProvenance,
  rawPayloadProvenance,
  refreshCanonicalFacts,
  refreshCompany,
  refreshPayload,
 )

runTests :: IO ()
runTests = do
  batch <- case decodeFmpStatements incomeFixture cashFixture retrievedAt (Symbol "TCS") (Symbol "TCS.NS") of
    Left failure -> fail ("valid FMP fixtures rejected: " <> show failure)
    Right value -> pure value
  let facts = refreshCanonicalFacts batch
  assert
    "all supported statement concepts are normalized"
    ( map canonicalConcept facts
        == [ "revenue"
           , "gross_profit"
           , "operating_profit"
           , "net_profit"
           , "operating_cash_flow"
           , "capital_expenditure"
           ]
    )
  assert
    "provider decimal currency units are converted to minor units"
    (maybe False ((== 12345600) . canonicalAmountMinor) (find ((== "revenue") . canonicalConcept) facts))
  assert
    "negative capex values are normalized to positive outflows"
    ( maybe
        False
        ((== 400000) . canonicalAmountMinor)
        (find ((== "capital_expenditure") . canonicalConcept) facts)
    )
  assert
    "fiscal annual period start is inferred from fiscal period end"
    ( maybe
        False
        ((== fromGregorian 2025 4 1) . canonicalPeriodStart)
        (find ((== "revenue") . canonicalConcept) facts)
    )
  assert "annual period type is retained" (all ((== Annual) . canonicalPeriodType) facts)
  assert
    "source is identified as FMP"
    (provenanceSource (rawPayloadProvenance (refreshPayload batch)) == SourceId "FINANCIAL_MODELING_PREP")
  assert "provider symbol maps to the local ticker" (refreshCompany batch == CompanyId "TCS")
  case find ((== "revenue") . canonicalConcept) facts of
    Just revenue ->
      assert
        "fact provenance retains the precise FMP endpoint without its secret key"
        (maybe False (Text.isInfixOf "symbol=TCS.NS") (provenanceUri (canonicalProvenance revenue)))
    Nothing -> fail "FMP revenue fixture fact missing"
  assert
    "invalid provider date fails explicitly"
    ( isLeft
        ( decodeFmpStatements
            "[{\"date\":\"bad\",\"period\":\"FY\"}]"
            cashFixture
            retrievedAt
            (Symbol "TCS")
            (Symbol "TCS.NS")
        )
    )
  assert
    "no usable rows fails explicitly"
    (isLeft (decodeFmpStatements "[]" "[]" retrievedAt (Symbol "TCS") (Symbol "TCS.NS")))
  assert
    "unsupported currency fails explicitly"
    ( isLeft
        (decodeFmpStatements unsupportedCurrency cashFixture retrievedAt (Symbol "TCS") (Symbol "TCS.NS"))
    )
  missingKey <- fetchFmpStatements "" (Symbol "TCS") (Symbol "TCS.NS")
  assert "missing API key fails before network access" (isLeft missingKey)
  avBatch <- case decodeAlphaVantageStatements
    avIncomeFixture
    avCashFixture
    retrievedAt
    (Symbol "TCS")
    (Symbol "TCS.BSE") of
    Left failure -> fail ("valid Alpha Vantage fixtures rejected: " <> show failure)
    Right value -> pure value
  assert
    "Alpha Vantage annual and quarterly statements normalize"
    (length (refreshCanonicalFacts avBatch) == 12)
  assert
    "Alpha Vantage raw source is preserved and identified"
    (provenanceSource (rawPayloadProvenance (refreshPayload avBatch)) == SourceId "ALPHA_VANTAGE")
  avMissingKey <- fetchAlphaVantageStatements "" (Symbol "TCS") (Symbol "TCS.BSE")
  assert "missing Alpha Vantage key fails before network access" (isLeft avMissingKey)
  assert
    "Alpha Vantage quota response fails explicitly"
    ( isLeft
        ( decodeAlphaVantageStatements
            "{\"Note\":\"API call frequency exceeded\"}"
            avCashFixture
            retrievedAt
            (Symbol "TCS")
            (Symbol "TCS.BSE")
        )
    )
  putStrLn "FMP statement adapter tests passed."

incomeFixture :: ByteString.ByteString
incomeFixture =
  "[{\"date\":\"2026-03-31\",\"period\":\"FY\",\"reportedCurrency\":\"INR\",\"revenue\":123456.00,\"grossProfit\":30000,\"operatingIncome\":20000,\"netIncome\":15000}]"

cashFixture :: ByteString.ByteString
cashFixture =
  "[{\"date\":\"2026-03-31\",\"period\":\"FY\",\"reportedCurrency\":\"INR\",\"operatingCashFlow\":18000,\"capitalExpenditure\":-4000}]"

unsupportedCurrency :: ByteString.ByteString
unsupportedCurrency =
  "[{\"date\":\"2026-03-31\",\"period\":\"FY\",\"reportedCurrency\":\"JPY\",\"revenue\":100}]"

avIncomeFixture :: ByteString.ByteString
avIncomeFixture =
  "{\"annualReports\":[{\"fiscalDateEnding\":\"2026-03-31\",\"reportedCurrency\":\"INR\",\"totalRevenue\":\"100000\",\"grossProfit\":\"30000\",\"operatingIncome\":\"20000\",\"netIncome\":\"15000\"}],\"quarterlyReports\":[{\"fiscalDateEnding\":\"2025-12-31\",\"reportedCurrency\":\"INR\",\"totalRevenue\":\"25000\",\"grossProfit\":\"7500\",\"operatingIncome\":\"5000\",\"netIncome\":\"3750\"}]}"

avCashFixture :: ByteString.ByteString
avCashFixture =
  "{\"annualReports\":[{\"fiscalDateEnding\":\"2026-03-31\",\"reportedCurrency\":\"INR\",\"operatingCashflow\":\"18000\",\"capitalExpenditures\":\"-4000\"}],\"quarterlyReports\":[{\"fiscalDateEnding\":\"2025-12-31\",\"reportedCurrency\":\"INR\",\"operatingCashflow\":\"4500\",\"capitalExpenditures\":\"-1000\"}]}"

retrievedAt :: UTCTime
retrievedAt = UTCTime (fromGregorian 2026 10 20) (secondsToDiffTime 3600)

isLeft :: Either a b -> Bool
isLeft result = case result of
  Left _ -> True
  Right _ -> False

assert :: String -> Bool -> IO ()
assert label passed = if passed then pure () else fail ("FMP adapter assertion failed: " <> label)

{-# LANGUAGE OverloadedStrings #-}

module Seby.Phase7Spec (runTests) where

import Data.ByteString.Char8 qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Seby.Analysis.ReportBuilder (buildCompanyReport)
import Seby.Domain.Types (
  CompanyId (..),
  DataIssue (..),
  Provenance (..),
  SourceId (..),
  StatementBasis (..),
  Symbol (..),
 )
import Seby.Ingestion.LocalStatements (decodeStatementImport)
import Seby.Persistence.SQLite (payloadSha256)
import Seby.Persistence.Types (CanonicalFact (..), RawPayload (..), RefreshBatch (..))
import Seby.Reporting.Markdown (renderMarkdown)
import Seby.Reporting.Model (reportIssues)
import Seby.Reporting.Terminal (renderTerminal)

runTests :: IO ()
runTests = do
  batch <- case decodeStatementImport fixture importTime of
    Left errorValue -> fail ("valid local statement import rejected: " <> Text.unpack errorValue)
    Right value -> pure value
  assert
    "manual import identifies ticker as stable company ID"
    (refreshCompany batch == CompanyId "TCS")
  assert
    "manual import retains company name"
    (refreshCompanyName batch == "Tata Consultancy Services")
  assert
    "manual import retains exact source payload digest"
    (provenanceDigest (rawPayloadProvenance (refreshPayload batch)) == payloadSha256 fixture)
  assert
    "all imported facts retain source lineage"
    ( all
        ( (== provenanceSource (rawPayloadProvenance (refreshPayload batch)))
            . provenanceSource
            . canonicalProvenance
        )
        (refreshCanonicalFacts batch)
    )
  assert "invalid JSON fails explicitly" (isLeft (decodeStatementImport "{broken" importTime))
  let nonHttpsFixture =
        ByteString.pack . Text.unpack $
          Text.replace
            "https://filings.example.test/tcs.pdf"
            "http://filings.example.test/tcs.pdf"
            (Text.pack (ByteString.unpack fixture))
  assert
    "unverified non-HTTPS citation is rejected"
    (isLeft (decodeStatementImport nonHttpsFixture importTime))
  let source = rawPayloadProvenance (refreshPayload batch)
      facts = refreshCanonicalFacts batch
  report <- case buildCompanyReport
    (fromGregorian 2026 10 20)
    (CompanyId "TCS")
    (Symbol "TCS")
    (refreshCompanyName batch)
    Consolidated
    "group"
    facts of
    Left errorValue -> fail ("source facts did not produce a report: " <> Text.unpack errorValue)
    Right value -> pure value
  let terminal = renderTerminal report
      markdown = renderMarkdown report
  assert
    "terminal report includes sourced revenue"
    ("Revenue:" `Text.isInfixOf` terminal && "₹10000.00 INR" `Text.isInfixOf` terminal)
  assert
    "free cash flow uses operating cash flow less positive capex"
    ("Free cash flow: ₹1400.00 INR" `Text.isInfixOf` terminal)
  assert
    "operating margin is calculated from compatible facts"
    ("Operating margin: 20.0%" `Text.isInfixOf` terminal)
  assert
    "sourced report carries original filing URL"
    ("https://filings.example.test/tcs.pdf" `Text.isInfixOf` markdown)
  assert
    "sourced report carries imported payload digest"
    (provenanceDigest source `Text.isInfixOf` markdown)
  assert "report clearly labels selected scope" ("Scope: group" `Text.isInfixOf` terminal)
  case facts of
    firstFact : _ -> do
      let conflictingFact =
            firstFact
              { canonicalAmountMinor = canonicalAmountMinor firstFact + 100
              , canonicalProvenance =
                  source{provenanceSource = SourceId "SECOND_SOURCE", provenanceDigest = "second-source-digest"}
              }
      conflictingReport <- case buildCompanyReport
        (fromGregorian 2026 10 20)
        (CompanyId "TCS")
        (Symbol "TCS")
        (refreshCompanyName batch)
        Consolidated
        "group"
        (facts <> [conflictingFact]) of
        Left errorValue -> fail ("conflicting sources prevented report creation: " <> Text.unpack errorValue)
        Right value -> pure value
      assert
        "cross-source value conflicts are surfaced in report quality section"
        (any isConflict (reportIssues conflictingReport))
    [] -> fail "fixture unexpectedly has no financial facts"
  putStrLn "Phase 7 sourced import and report tests passed."

fixture :: ByteString.ByteString
fixture =
  ByteString.pack
    "{\"symbol\":\"tcs\",\"company_name\":\"Tata Consultancy Services\",\"source_id\":\"COMPANY_FILINGS\",\"source_uri\":\"https://filings.example.test/tcs.pdf\",\"published_at\":\"2026-05-01T00:00:00Z\",\"facts\":[{\"concept\":\"revenue\",\"period_start\":\"2025-04-01\",\"period_end\":\"2026-03-31\",\"period_type\":\"annual\",\"currency\":\"INR\",\"amount_minor\":1000000,\"basis\":\"consolidated\",\"scope\":\"group\"},{\"concept\":\"operating_profit\",\"period_start\":\"2025-04-01\",\"period_end\":\"2026-03-31\",\"period_type\":\"annual\",\"currency\":\"INR\",\"amount_minor\":200000,\"basis\":\"consolidated\",\"scope\":\"group\"},{\"concept\":\"net_profit\",\"period_start\":\"2025-04-01\",\"period_end\":\"2026-03-31\",\"period_type\":\"annual\",\"currency\":\"INR\",\"amount_minor\":150000,\"basis\":\"consolidated\",\"scope\":\"group\"},{\"concept\":\"operating_cash_flow\",\"period_start\":\"2025-04-01\",\"period_end\":\"2026-03-31\",\"period_type\":\"annual\",\"currency\":\"INR\",\"amount_minor\":180000,\"basis\":\"consolidated\",\"scope\":\"group\"},{\"concept\":\"capital_expenditure\",\"period_start\":\"2025-04-01\",\"period_end\":\"2026-03-31\",\"period_type\":\"annual\",\"currency\":\"INR\",\"amount_minor\":40000,\"basis\":\"consolidated\",\"scope\":\"group\"},{\"concept\":\"gross_profit\",\"period_start\":\"2025-04-01\",\"period_end\":\"2026-03-31\",\"period_type\":\"annual\",\"currency\":\"INR\",\"amount_minor\":300000,\"basis\":\"consolidated\",\"scope\":\"group\"}]}"

importTime :: UTCTime
importTime = UTCTime (fromGregorian 2026 10 20) (secondsToDiffTime 3600)

isConflict :: DataIssue -> Bool
isConflict issue = case issue of
  ConflictingSources sources ->
    SourceId "COMPANY_FILINGS" `elem` toList sources && SourceId "SECOND_SOURCE" `elem` toList sources
  _ -> False

toList :: NonEmpty a -> [a]
toList (first :| rest) = first : rest

isLeft :: Either a b -> Bool
isLeft result = case result of
  Left _ -> True
  Right _ -> False

assert :: String -> Bool -> IO ()
assert label passed =
  if passed
    then pure ()
    else fail ("Phase 7 assertion failed: " <> label)

{-# LANGUAGE OverloadedStrings #-}

module Seby.PersistenceSpec (runTests) where

import Control.Exception (bracket)
import Control.Monad (when, (>=>))
import Data.ByteString.Char8 qualified as ByteString
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Data.Time.Clock (addUTCTime)
import Database.SQLite.Simple (Only (..), close, execute, execute_, open, query, query_)
import Seby.Analysis.CorporateActions (CorporateActionType (..), mkCorporateAction)
import Seby.Analysis.FactViews (FactView (..), selectFactView)
import Seby.Analysis.ReportBuilder (buildCompanyReport)
import Seby.Domain.Money (Currency (..))
import Seby.Domain.Types (
  CompanyId (..),
  PeriodType (..),
  Provenance (..),
  SourceId (..),
  StatementBasis (..),
  Symbol (..),
 )
import Seby.Persistence.Repository
import Seby.Persistence.SQLite
import Seby.Persistence.Types
import Seby.Reporting.Terminal (renderTerminal)
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)

runTests :: IO ()
runTests = do
  testSha256
  testCacheDecisionTable
  withFreshDatabase testFutureSchemaRejected
  withFreshDatabase testV1ToV2Migration
  withFreshDatabase testOfflineReportReproduction
  withFreshDatabase testRepositoryRoundTrip
  withFreshDatabase testWatchlistStress
  putStrLn "✓ Phase 2 persistence integration and boundary tests passed."

testSha256 :: IO ()
testSha256 =
  assertEqual
    "SHA-256 uses the standard lowercase hexadecimal encoding"
    "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    (payloadSha256 "abc")

testCacheDecisionTable :: IO ()
testCacheDecisionTable = do
  let at = timestamp 2025 1 1
  assertEqual "prefer cache hit" UseCachedData (decideCache PreferCache (CacheHit at))
  assertEqual
    "prefer stale cache and refresh with fallback"
    RefreshThenAllowCachedFallback
    (decideCache PreferCache (CacheStale at))
  assertEqual
    "prefer cache miss refreshes with fallback"
    RefreshThenAllowCachedFallback
    (decideCache PreferCache CacheMiss)
  assertEqual
    "required refresh bypasses cache"
    RefreshBeforeUse
    (decideCache RefreshRequired (CacheHit at))
  assertEqual
    "required refresh also bypasses stale data"
    RefreshBeforeUse
    (decideCache RefreshRequired (CacheStale at))
  assertEqual "offline hit" UseCachedData (decideCache OfflineOnly (CacheHit at))
  assertEqual
    "offline stale data is explicit"
    UseStaleDataWithWarning
    (decideCache OfflineOnly (CacheStale at))
  assertEqual
    "offline miss fails explicitly"
    OfflineDataUnavailable
    (decideCache OfflineOnly CacheMiss)
  assertEqual
    "age inside policy remains a cache hit"
    (CacheHit at)
    (cacheStatusAt (addUTCTime 60 at) 60 (Just at))
  assertEqual
    "age outside policy is stale"
    (CacheStale at)
    (cacheStatusAt (addUTCTime 61 at) 60 (Just at))
  assertEqual
    "future timestamp is treated as stale"
    (CacheStale (addUTCTime 1 at))
    (cacheStatusAt at 60 (Just (addUTCTime 1 at)))
  assertEqual "no successful cache entry is a miss" CacheMiss (cacheStatusAt at 60 Nothing)

testFutureSchemaRejected :: FilePath -> IO ()
testFutureSchemaRejected path = do
  connection <- open path
  execute_
    connection
    "CREATE TABLE schema_migrations (version INTEGER PRIMARY KEY, applied_at TEXT NOT NULL)"
  execute_
    connection
    "INSERT INTO schema_migrations(version, applied_at) VALUES (99, '2025-01-01T00:00:00Z')"
  close connection
  result <- openFactRepository path
  assert "newer database schema is rejected with a typed error" $
    case result of
      Left (UnsupportedSchemaVersion 99) -> True
      _ -> False

testV1ToV2Migration :: FilePath -> IO ()
testV1ToV2Migration path = do
  connection <- open path
  execute_
    connection
    "CREATE TABLE schema_migrations (version INTEGER PRIMARY KEY, applied_at TEXT NOT NULL)"
  execute
    connection
    "INSERT INTO schema_migrations(version, applied_at) VALUES (1, ?)"
    (Only ("2025-01-01T00:00:00Z" :: Text))
  close connection
  opened <- openFactRepository path
  repository <- expectRight "migrate v1 database to v2" opened
  addWatchlistSymbol repository (Symbol "INFY") >>= expectRight "use watchlist after migration"
  assertEqual "v1 data model migrates to v2" [Symbol "INFY"]
    =<< (loadWatchlist repository >>= expectRight "load migrated watchlist")
  closeFactRepository repository

testOfflineReportReproduction :: FilePath -> IO ()
testOfflineReportReproduction path = do
  repository <- openFactRepository path >>= expectRight "open repository for offline reproduction"
  let company = CompanyId "company-1"
      ticker = Symbol "TCS"
      batch = makeBatch "offline-replay" "offline reproducibility fixture" 1250000 (timestamp 2025 4 1)
      reportDate = fromGregorian 2026 10 21
  expectedReport <-
    expectRight
      "build report from the original validated fixture"
      ( buildCompanyReport
          reportDate
          company
          ticker
          "Test Company"
          Consolidated
          "group"
          (refreshCanonicalFacts batch)
      )
  storeRefreshBatch repository batch >>= expectRight "persist fixture for offline replay"
  closeFactRepository repository

  reopened <- openFactRepository path >>= expectRight "reopen repository without network access"
  facts <- loadCurrentCompanyFacts reopened company >>= expectRight "read cached facts offline"
  name <- loadCompanyName reopened company >>= expectRight "read cached company identity offline"
  assertEqual "offline replay retains company identity" (Just "Test Company") name
  actualReport <-
    expectRight
      "rebuild report from reopened SQLite facts"
      (buildCompanyReport reportDate company ticker "Test Company" Consolidated "group" facts)
  assertEqual
    "offline re-analysis after database reopen reproduces the complete report"
    (renderTerminal expectedReport)
    (renderTerminal actualReport)
  assertEqual
    "offline replay retains source payload digest"
    [provenanceDigest (rawPayloadProvenance (refreshPayload batch))]
    (map (provenanceDigest . canonicalProvenance) facts)
  closeFactRepository reopened

testWatchlistStress :: FilePath -> IO ()
testWatchlistStress path = do
  opened <- openFactRepository path
  repository <- expectRight "open repository for watchlist stress" opened
  let symbols = [Symbol ("TICKER" <> Text.pack (show number)) | number <- [1 .. 500 :: Int]]
  mapM_ (addWatchlistSymbol repository >=> expectRight "persist stress watchlist item") symbols
  mapM_ (addWatchlistSymbol repository >=> expectRight "idempotent stress watchlist replay") symbols
  loaded <- loadWatchlist repository >>= expectRight "reload stress watchlist"
  assertEqual "large watchlist remains deduplicated and sorted" (length symbols) (length loaded)
  assertEqual "large watchlist starts in symbol order" (take 1 symbols) (take 1 loaded)
  checkDatabaseIntegrity repository >>= expectRight "check stressed database integrity"
  closeFactRepository repository

testRepositoryRoundTrip :: FilePath -> IO ()
testRepositoryRoundTrip path = do
  opened <- openFactRepository path
  repository <- expectRight "open and migrate current schema" opened
  assertEqual "schema version" 2 currentSchemaVersion
  let first = makeBatch "run-1" "original filing payload" (10 ^ (30 :: Int)) (timestamp 2025 4 1)
      reprocessed =
        first
          { refreshRunId = "run-1-reprocessed"
          , refreshRawObservations =
              map (\raw -> raw{rawObservationExtractorVersion = "extractor-v2"}) (refreshRawObservations first)
          , refreshCanonicalFacts =
              map (\fact -> fact{canonicalPolicyVersion = "canonical-v2"}) (refreshCanonicalFacts first)
          }
      second = makeBatch "run-2" "corrected filing payload" 42 (addUTCTime 3600.123456 (timestamp 2025 4 1))
      secondary =
        withSource
          (SourceId "BSE")
          (makeBatch "run-3" "secondary-source filing payload" 41 (addUTCTime 7200 (timestamp 2025 4 1)))
      lateOldFiling =
        withPublishedAt
          (timestamp 2024 3 31)
          (makeBatch "run-4" "late arrival with old publication" 29 (addUTCTime 10800 (timestamp 2025 4 1)))
      period = FactPeriod (fromGregorian 2024 4 1) (fromGregorian 2025 3 31) Annual
      company = CompanyId "company-1"
  planConnection <- open path
  queryPlan <-
    query
      planConnection
      "EXPLAIN QUERY PLAN SELECT fact_id FROM canonical_fact WHERE company_id = ? AND period_start = ? AND period_end = ? AND period_type = ?"
      (("company-1", "2024-04-01", "2025-03-31", "annual") :: (Text, Text, Text, Text)) ::
      IO [(Int, Int, Int, Text)]
  close planConnection
  assert "canonical fact index serves the cache lookup" $
    any (Text.isInfixOf "canonical_fact_lookup" . fourthColumn) queryPlan
  storeRefreshBatch repository first >>= expectRight "store initial refresh atomically"
  assertEqual "company display name is persisted with facts" (Just "Test Company")
    =<< (loadCompanyName repository company >>= expectRight "load company display name")
  currentCompanyFacts <-
    loadCurrentCompanyFacts repository company >>= expectRight "load current facts across all periods"
  assertEqual
    "current-company fact lookup returns persisted facts"
    [10 ^ (30 :: Int)]
    (map canonicalAmountMinor currentCompanyFacts)
  addWatchlistSymbol repository (Symbol " tcs ") >>= expectRight "add normalized watchlist symbol"
  addWatchlistSymbol repository (Symbol "TCS") >>= expectRight "adding watchlist symbol is idempotent"
  assertEqual "watchlist normalizes and deduplicates symbols" [Symbol "TCS"]
    =<< (loadWatchlist repository >>= expectRight "load watchlist")
  removeWatchlistSymbol repository (Symbol "tcs")
    >>= expectRight "remove watchlist symbol case-insensitively"
  assertEqual "watchlist removal is idempotent" []
    =<< (loadWatchlist repository >>= expectRight "load empty watchlist")
  assertEqual "invalid whitespace ticker is rejected" (Left (InvalidWatchlistSymbol "bad ticker"))
    =<< addWatchlistSymbol repository (Symbol "bad ticker")
  checkDatabaseIntegrity repository >>= expectRight "check SQLite integrity"
  vacuumDatabase repository >>= expectRight "vacuum SQLite database"
  action <-
    expectRight
      "construct persisted split action"
      ( mkCorporateAction
          "split-2-for-1"
          company
          (fromGregorian 2025 7 1)
          (ShareSplit (2 % 1))
          (rawPayloadProvenance (refreshPayload first))
      )
  persistCorporateActions repository [action] >>= expectRight "persist corporate-action ledger"
  loadedActions <-
    loadCorporateActions repository company >>= expectRight "reload corporate-action ledger"
  assertEqual "corporate-action ledger round trips" [action] loadedActions
  storeRefreshBatch repository (first{refreshRunId = "run-1-replay"})
    >>= expectRight "replay identical payload idempotently"
  storeRefreshBatch repository reprocessed
    >>= expectRight "retain extractor and canonical-policy revisions"
  initialFacts <-
    loadCanonicalFacts repository company period >>= expectRight "load original annual fact"
  assertEqual
    "first canonical value retains arbitrary precision"
    [10 ^ (30 :: Int)]
    (map canonicalAmountMinor initialFacts)
  initialFact <- onlyFact "one source fact is available after initial refresh" initialFacts
  assertEqual
    "canonical policy version is persisted"
    "canonical-v2"
    (canonicalPolicyVersion initialFact)
  assertEqual
    "provenance survives persistence"
    (rawPayloadProvenance (refreshPayload first))
    (canonicalProvenance initialFact)
  storeRefreshBatch repository second >>= expectRight "store correction as a new version"
  latestFacts <-
    loadCanonicalFacts repository company period >>= expectRight "load latest version of fact"
  assertEqual "latest fact is selected after a correction" [42] (map canonicalAmountMinor latestFacts)
  factHistory <-
    loadCanonicalFactHistory repository company period >>= expectRight "load immutable fact history"
  assertEqual
    "fact history retains both policy and source versions"
    [10 ^ (30 :: Int), 10 ^ (30 :: Int), 42]
    (map canonicalAmountMinor factHistory)
  assertEqual
    "reported view selects the first published value"
    [10 ^ (30 :: Int)]
    (map canonicalAmountMinor (selectFactView AsReportedFacts factHistory))
  assertEqual
    "restated view selects the latest published value"
    [42]
    (map canonicalAmountMinor (selectFactView RestatedFacts factHistory))
  assertEqual
    "point-in-time view selects the latest version known by the cutoff"
    [10 ^ (30 :: Int)]
    (map canonicalAmountMinor (selectFactView (FactsKnownAt (timestamp 2025 3 31)) factHistory))
  latestFact <- onlyFact "one source fact is available after correction" latestFacts
  assertEqual
    "latest value retains new source digest"
    (provenanceDigest (rawPayloadProvenance (refreshPayload second)))
    (provenanceDigest (canonicalProvenance latestFact))
  assertEqual
    "subsecond provenance timestamp round trips exactly"
    (provenanceRetrievedAt (rawPayloadProvenance (refreshPayload second)))
    (provenanceRetrievedAt (canonicalProvenance latestFact))
  storeRefreshBatch repository secondary
    >>= expectRight "store independent secondary-source observation"
  multiSourceFacts <-
    loadCanonicalFacts repository company period >>= expectRight "load all current source observations"
  assertEqual
    "separate sources remain separately traceable"
    [41, 42]
    (map canonicalAmountMinor multiSourceFacts)
  storeRefreshBatch repository lateOldFiling
    >>= expectRight "store late-arriving older source revision"
  afterOutOfOrder <-
    loadCanonicalFacts repository company period >>= expectRight "load after out-of-order filing"
  assertEqual
    "older publication does not replace a later source revision"
    [41, 42]
    (map canonicalAmountMinor afterOutOfOrder)

  let tampered = makeBatch "run-tampered" "payload bytes" 99 (addUTCTime 7200 (timestamp 2025 4 1))
      originalProvenance = rawPayloadProvenance (refreshPayload tampered)
      tamperedPayload = (refreshPayload tampered){rawPayloadProvenance = originalProvenance{provenanceDigest = "0"}}
      badDigestBatch = tampered{refreshPayload = tamperedPayload}
  digestResult <- storeRefreshBatch repository badDigestBatch
  assert
    "payload digest mismatch is rejected"
    (case digestResult of Left InvalidPayloadDigest{} -> True; _ -> False)
  afterTamper <-
    loadCanonicalFacts repository company period >>= expectRight "load after rejected digest"
  assertEqual "digest rejection does not mutate facts" [41, 42] (map canonicalAmountMinor afterTamper)
  let oversizedBytes = ByteString.replicate (maxPayloadBytes + 1) '\0'
      oversizedPayload = (refreshPayload tampered){rawPayloadBytes = oversizedBytes}
      oversizedBatch = tampered{refreshPayload = oversizedPayload}
  oversizedResult <- storeRefreshBatch repository oversizedBatch
  assert "oversized raw payload is rejected before persistence" $
    case oversizedResult of
      Left (PayloadTooLarge actual limit) -> actual == toInteger (maxPayloadBytes + 1) && limit == toInteger maxPayloadBytes
      _ -> False

  -- Force an abort after payload and raw-observation inserts. The whole write
  -- transaction must roll back, including those earlier inserts.
  connection <- open path
  execute_
    connection
    "CREATE TRIGGER reject_canonical BEFORE INSERT ON canonical_fact BEGIN SELECT RAISE(ABORT, 'test fault injection'); END"
  let failing = makeBatch "run-failed" "must roll back completely" 77 (addUTCTime 14400 (timestamp 2025 4 1))
  failedResult <- storeRefreshBatch repository failing
  assert
    "SQL interruption is reported"
    (case failedResult of Left (DatabaseFailure _) -> True; _ -> False)
  execute_ connection "DROP TRIGGER reject_canonical"
  close connection
  afterFailure <-
    loadCanonicalFacts repository company period >>= expectRight "load after failed transaction"
  assertEqual
    "failed refresh preserves the preceding canonical snapshot"
    [41, 42]
    (map canonicalAmountMinor afterFailure)
  counts <- open path
  payloadRows <- query_ counts "SELECT COUNT(*) FROM raw_payload" :: IO [Only Int]
  rawPayloadBodies <-
    query
      counts
      "SELECT body FROM raw_payload WHERE source_id = ? AND digest = ?"
      (("NSE", provenanceDigest (rawPayloadProvenance (refreshPayload first))) :: (Text, Text)) ::
      IO [Only ByteString.ByteString]
  rawRows <- query_ counts "SELECT COUNT(*) FROM raw_fact" :: IO [Only Int]
  extractorVersions <-
    query_ counts "SELECT COUNT(DISTINCT extractor_version) FROM raw_fact" :: IO [Only Int]
  canonicalRows <- query_ counts "SELECT COUNT(*) FROM canonical_fact" :: IO [Only Int]
  policyVersions <-
    query_ counts "SELECT COUNT(DISTINCT policy_version) FROM canonical_fact" :: IO [Only Int]
  close counts
  assertEqual "failed refresh payload is rolled back" [Only 4] payloadRows
  assertEqual "failed refresh raw observations retain extractor revisions" [Only 5] rawRows
  assertEqual
    "raw payload bytes are retained exactly"
    [Only (rawPayloadBytes (refreshPayload first))]
    rawPayloadBodies
  assertEqual "re-extraction versions are immutable" [Only 2] extractorVersions
  assertEqual "older canonical versions remain stored" [Only 5] canonicalRows
  assertEqual "canonical policy versions are immutable" [Only 2] policyVersions

  state <- getRefreshState repository company >>= expectRight "read latest refresh status"
  assertEqual
    "failed attempt remains visible"
    (Just (refreshAttemptedAt failing))
    (lastAttemptAt <$> state)
  assertEqual
    "last success remains separately visible"
    (Just (refreshCompletedAt lateOldFiling))
    (state >>= lastSuccessfulAt)
  assert
    "latest refresh exposes failure reason"
    ( case lastFailureReason =<< state of
        Just reason -> "test fault injection" `Text.isInfixOf` reason
        Nothing -> False
    )

  closeFactRepository repository
  reopened <- openFactRepository path >>= expectRight "reopen and re-run migrations"
  migratedFacts <-
    loadCanonicalFacts reopened company period >>= expectRight "load facts after reopening"
  assertEqual
    "migration is idempotent and preserves facts"
    [41, 42]
    (map canonicalAmountMinor migratedFacts)
  closeFactRepository reopened

makeBatch :: Text -> ByteString.ByteString -> Integer -> UTCTime -> RefreshBatch
makeBatch runId bytes amount retrievedAt =
  let provenance =
        Provenance
          { provenanceSource = SourceId "NSE"
          , provenanceUri = Just "https://example.invalid/filing"
          , provenanceRetrievedAt = retrievedAt
          , provenancePublishedAt = Just (addUTCTime (-86400) retrievedAt)
          , provenanceDigest = payloadSha256 bytes
          }
      company = CompanyId "company-1"
   in RefreshBatch
        { refreshRunId = runId
        , refreshCompany = company
        , refreshCompanyName = "Test Company"
        , refreshAttemptedAt = retrievedAt
        , refreshCompletedAt = retrievedAt
        , refreshPayload = RawPayload provenance bytes
        , refreshRawObservations =
            [ RawObservation
                "Revenue"
                (Text.pack (show amount))
                "INR paise"
                "extractor-v1"
                (fromGregorian 2024 4 1)
                (fromGregorian 2025 3 31)
            ]
        , refreshCanonicalFacts =
            [ CanonicalFact
                company
                "revenue"
                (fromGregorian 2024 4 1)
                (fromGregorian 2025 3 31)
                Annual
                INR
                amount
                Consolidated
                "group"
                "canonical-v1"
                provenance
            ]
        }

withSource :: SourceId -> RefreshBatch -> RefreshBatch
withSource source batch =
  let oldProvenance = rawPayloadProvenance (refreshPayload batch)
      newProvenance = oldProvenance{provenanceSource = source}
      payload = (refreshPayload batch){rawPayloadProvenance = newProvenance}
      facts = map (\fact -> fact{canonicalProvenance = newProvenance}) (refreshCanonicalFacts batch)
   in batch{refreshPayload = payload, refreshCanonicalFacts = facts}

withPublishedAt :: UTCTime -> RefreshBatch -> RefreshBatch
withPublishedAt publishedAt batch =
  let oldProvenance = rawPayloadProvenance (refreshPayload batch)
      newProvenance = oldProvenance{provenancePublishedAt = Just publishedAt}
      payload = (refreshPayload batch){rawPayloadProvenance = newProvenance}
      facts = map (\fact -> fact{canonicalProvenance = newProvenance}) (refreshCanonicalFacts batch)
   in batch{refreshPayload = payload, refreshCanonicalFacts = facts}

fourthColumn :: (a, b, c, d) -> d
fourthColumn (_, _, _, value) = value

timestamp :: Integer -> Int -> Int -> UTCTime
timestamp year month day = UTCTime (fromGregorian year month day) (secondsToDiffTime 0)

withFreshDatabase :: (FilePath -> IO ()) -> IO ()
withFreshDatabase = bracket acquire cleanup
  where
    acquire = do
      directory <- getTemporaryDirectory
      (path, handle) <- openTempFile directory "seby-persistence-test.sqlite"
      hClose handle
      removeFile path
      pure path
    cleanup path = mapM_ removeIfPresent [path, path <> "-wal", path <> "-shm"]
    removeIfPresent path = do
      exists <- doesFileExist path
      when exists (removeFile path)

expectRight :: (Show e) => String -> Either e a -> IO a
expectRight label result = case result of
  Left err -> fail (label <> ": " <> show err)
  Right value -> pure value

assert :: String -> Bool -> IO ()
assert label condition = if condition then pure () else fail ("Assertion failed: " <> label)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
  assert (label <> "; expected " <> show expected <> ", got " <> show actual) (expected == actual)

onlyFact :: String -> [CanonicalFact] -> IO CanonicalFact
onlyFact label facts = case facts of
  [fact] -> pure fact
  _ -> fail (label <> "; expected exactly one fact, got " <> show (length facts))

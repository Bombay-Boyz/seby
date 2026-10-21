{-# LANGUAGE OverloadedStrings #-}

-- | Repeatable synthetic CPU baselines for report construction and batch use.
module Main (main) where

import Control.Exception (bracket)
import Control.Monad (forM_, replicateM)
import Data.ByteString.Char8 qualified as ByteString
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Text qualified as Text
import Data.Time (Day, UTCTime (..), fromGregorian, secondsToDiffTime)
import GHC.Clock (getMonotonicTimeNSec)
import Seby.Analysis.ReportBuilder (buildCompanyReport)
import Seby.Domain.Money (Currency (INR))
import Seby.Domain.Types (
  CompanyId (..),
  PeriodType (Annual),
  Provenance (..),
  SourceId (..),
  StatementBasis (Consolidated),
  Symbol (..),
 )
import Seby.Persistence.Repository (FactRepository (..))
import Seby.Persistence.SQLite (openFactRepository, payloadSha256)
import Seby.Persistence.Types (CanonicalFact (..), RawPayload (..), RefreshBatch (..))
import Seby.Reporting.Terminal (renderTerminal)
import System.Directory (doesFileExist, getFileSize, getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)
import System.Info (arch, compilerName, compilerVersion, os)

main :: IO ()
main = do
  putStrLn "Seby synthetic performance baseline"
  putStrLn
    ("compiler=" <> compilerName <> "-" <> show compilerVersion <> " os=" <> os <> " arch=" <> arch)
  putStrLn "timing=monotonic wall-clock; setup excluded; reports fully rendered"
  let singleFacts = factsForCompany 1 100
      batch = map (\company -> factsForCompany company 20) [1 .. 200]
  runCase "single-company-report" 50 (length singleFacts) (reportChecksum singleFacts)
  runCase "batch-200-company-reports" 3 (sum (map length batch)) (batchChecksum batch)
  withTempDatabase benchmarkPersistence

runCase :: String -> Int -> Int -> IO Int -> IO ()
runCase label iterations factCount action = do
  start <- getMonotonicTimeNSec
  checksum <- loop iterations 0
  end <- getMonotonicTimeNSec
  let elapsedNs = end - start
      perIterationMs = fromIntegral elapsedNs / fromIntegral iterations / 1_000_000 :: Double
  putStrLn
    ( label
        <> " iterations="
        <> show iterations
        <> " input_facts="
        <> show factCount
        <> " ms_per_iteration="
        <> show perIterationMs
        <> " checksum="
        <> show checksum
    )
  where
    loop 0 total = pure total
    loop remaining total = do
      result <- action
      result `seq` loop (remaining - 1) (total + result)

reportChecksum :: [CanonicalFact] -> IO Int
reportChecksum facts = pure $ case buildCompanyReport reportDay company symbol "Benchmark Co" Consolidated "group" facts of
  Left reason -> Text.length reason
  Right report -> Text.foldl' (\total character -> total + fromEnum character) 0 (renderTerminal report)
  where
    company = CompanyId "BENCH001"
    symbol = Symbol "BENCH001"

batchChecksum :: [[CanonicalFact]] -> IO Int
batchChecksum companyFacts = pure (foldl' addReport 0 companyFacts)
  where
    addReport total facts =
      total + case facts of
        firstFact : _ -> case buildCompanyReport
          reportDay
          (canonicalCompany firstFact)
          (Symbol "BENCH")
          "Benchmark Co"
          Consolidated
          "group"
          facts of
          Left reason -> Text.length reason
          Right report -> Text.foldl' (\count character -> count + fromEnum character) 0 (renderTerminal report)
        [] -> total

benchmarkPersistence :: FilePath -> IO ()
benchmarkPersistence path = do
  let company = CompanyId "BENCH900"
      facts = factsForCompany 900 100
      bytes = ByteString.pack "repeatable synthetic 600-fact database fixture"
      provenance = sourceProvenance{provenanceDigest = payloadSha256 bytes}
      batch = makeRefreshBatch "baseline-load" company facts bytes provenance
      largePayload = ByteString.replicate (4 * 1024 * 1024) 'x'
      largeProvenance = sourceProvenance{provenanceDigest = payloadSha256 largePayload}
      largeBatch = makeRefreshBatch "baseline-large-payload" company facts largePayload largeProvenance
  withRepository path $ \repository -> do
    storeRefreshBatch repository batch >>= either (fail . show) pure
    refreshCounter <- newIORef (0 :: Int)
    runCase "sqlite-identical-refresh-10x" 3 (length facts) $ do
      forM_ [1 .. 10 :: Int] $ \_ -> do
        number <- atomicModifyIORef' refreshCounter (\current -> (current + 1, current + 1))
        storeRefreshBatch repository (batch{refreshRunId = "baseline-refresh-" <> Text.pack (show number)})
          >>= either (fail . show) pure
      pure (length facts * 10)
    runCase "sqlite-large-payload-write" 1 (ByteString.length largePayload) $ do
      storeRefreshBatch repository largeBatch >>= either (fail . show) pure
      pure (ByteString.length largePayload)
  withRepository path $ \repository -> do
    runCase "sqlite-first-connection-analysis" 1 (length facts) (analysisChecksum repository company)
    runCase "sqlite-warm-100-analyses" 3 (length facts) $ do
      checksums <- replicateM 100 (analysisChecksum repository company)
      pure (sum checksums)
  databaseBytes <- fileSizeIfPresent path
  walBytes <- fileSizeIfPresent (path <> "-wal")
  putStrLn ("sqlite_database_bytes=" <> show databaseBytes <> " sqlite_wal_bytes=" <> show walBytes)
  putStrLn "large_payload_bytes=4194304 provider_requests=0 max_provider_concurrency=1"

makeRefreshBatch ::
  Text.Text -> CompanyId -> [CanonicalFact] -> ByteString.ByteString -> Provenance -> RefreshBatch
makeRefreshBatch runId company facts bytes provenance =
  RefreshBatch
    { refreshRunId = runId
    , refreshCompany = company
    , refreshCompanyName = "Benchmark Co"
    , refreshAttemptedAt = provenanceRetrievedAt provenance
    , refreshCompletedAt = provenanceRetrievedAt provenance
    , refreshPayload = RawPayload provenance bytes
    , refreshRawObservations = []
    , refreshCanonicalFacts = map (\fact -> fact{canonicalProvenance = provenance}) facts
    }

withRepository :: FilePath -> (FactRepository -> IO ()) -> IO ()
withRepository path action = do
  repository <- openFactRepository path >>= either (fail . show) pure
  bracket (pure repository) closeFactRepository action

analysisChecksum :: FactRepository -> CompanyId -> IO Int
analysisChecksum repository company = do
  facts <- loadCurrentCompanyFacts repository company >>= either (fail . show) pure
  report <-
    either
      (fail . Text.unpack)
      pure
      (buildCompanyReport reportDay company (Symbol "BENCH900") "Benchmark Co" Consolidated "group" facts)
  pure (Text.foldl' (\total character -> total + fromEnum character) 0 (renderTerminal report))

withTempDatabase :: (FilePath -> IO ()) -> IO ()
withTempDatabase action = bracket acquire cleanup action
  where
    acquire = do
      directory <- getTemporaryDirectory
      (path, handle) <- openTempFile directory "seby-performance.sqlite"
      hClose handle
      removeFile path
      pure path
    cleanup path = mapM_ removeIfPresent [path, path <> "-wal", path <> "-shm"]
    removeIfPresent path = do
      exists <- doesFileExist path
      if exists then removeFile path else pure ()

fileSizeIfPresent :: FilePath -> IO Integer
fileSizeIfPresent path = do
  exists <- doesFileExist path
  if exists then getFileSize path else pure 0

factsForCompany :: Int -> Int -> [CanonicalFact]
factsForCompany companyNumber yearCount =
  [ CanonicalFact
      { canonicalCompany = CompanyId (Text.pack ("BENCH" <> show companyNumber))
      , canonicalConcept = concept
      , canonicalPeriodStart = fromGregorian year 4 1
      , canonicalPeriodEnd = fromGregorian (year + 1) 3 31
      , canonicalPeriodType = Annual
      , canonicalCurrency = INR
      , canonicalAmountMinor = toInteger companyNumber * year * conceptIndex * 1000
      , canonicalStatementBasis = Consolidated
      , canonicalScope = "group"
      , canonicalPolicyVersion = "synthetic-benchmark-v1"
      , canonicalProvenance = sourceProvenance
      }
  | year <- take yearCount [fromIntegral (2026 - yearCount) .. 2025]
  , (conceptIndex, concept) <- zip [1 :: Integer ..] supportedConcepts
  ]

supportedConcepts :: [Text.Text]
supportedConcepts =
  [ "revenue"
  , "operating_profit"
  , "net_profit"
  , "operating_cash_flow"
  , "capital_expenditure"
  , "gross_profit"
  ]

sourceProvenance :: Provenance
sourceProvenance =
  Provenance
    (SourceId "SYNTHETIC_BENCHMARK")
    (Just "https://benchmark.invalid/fixture")
    (UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0))
    Nothing
    "synthetic-fixture-digest"

reportDay :: Day
reportDay = fromGregorian 2026 10 21

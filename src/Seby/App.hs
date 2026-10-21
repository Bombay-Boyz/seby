{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Application orchestration and top-level command execution.
module Seby.App (
  Command (..),
  runCommand,
) where

import Control.Exception (IOException, bracket, try)
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Data.Time (getCurrentTime, utctDay)
import Seby.Analysis.ReportBuilder (buildCompanyReport)
import Seby.Domain.Types (CompanyId (..), StatementBasis, Symbol (..))
import Seby.Ingestion.FundamentalProviders (
  FmpError (..),
  fetchAlphaVantageStatements,
  fetchFmpStatements,
 )
import Seby.Ingestion.LocalStatements (decodeStatementImport)
import Seby.Persistence.Repository (FactRepository (..))
import Seby.Persistence.SQLite (openFactRepository)
import Seby.Persistence.Types (RefreshBatch (..), StoreError)
import Seby.Reporting.Model (Report)
import Seby.Reporting.Terminal (renderTerminal)
import System.Environment (lookupEnv)
import System.Exit (die, exitFailure)
import System.IO (hFlush, stderr, stdout)

-- | Supported CLI commands for Seby.
data Command
  = AnalyseCompany !Symbol !StatementBasis !Text
  | AnalyseWatchlist !StatementBasis !Text
  | ImportStatements !FilePath
  | RefreshStatements !Symbol ![(Text, Symbol)]
  | InvalidAnalysisRequest !Text
  | WatchlistAdd !Symbol
  | WatchlistRemove !Symbol
  | WatchlistList
  | DatabaseCheck
  | DatabaseVacuum
  | DatabaseBackup !FilePath
  | PrintVersion
  deriving stock (Eq, Show)

-- | Execute a parsed CLI command.
runCommand :: Command -> IO ()
runCommand cmd = case cmd of
  AnalyseCompany symbol basis scope -> withRepository $ \repository -> do
    report <- loadReport repository symbol basis scope
    case report of
      Left errorValue -> failText errorValue
      Right value -> TIO.putStr (renderTerminal value)
  AnalyseWatchlist basis scope -> withRepository $ \repository -> do
    symbols <- loadWatchlist repository >>= either (failStore) pure
    if null symbols
      then failText "The local watchlist is empty; add symbols with `seby watchlist add SYMBOL`."
      else do
        results <- traverse (\symbol -> (,) symbol <$> loadReport repository symbol basis scope) symbols
        let failures = [(symbol, reason) | (symbol, Left reason) <- results]
        mapM_ emitBatchResult [(symbol, Right report) | (symbol, Right report) <- results]
        hFlush stdout
        mapM_ emitBatchResult [(symbol, Left reason) | (symbol, Left reason) <- results]
        if null failures
          then pure ()
          else do
            TIO.hPutStrLn
              stderr
              ( Text.pack (show (length failures))
                  <> " of "
                  <> Text.pack (show (length symbols))
                  <> " watchlist reports failed."
              )
            exitFailure
  ImportStatements path -> withRepository $ \repository -> do
    input <- try @IOException (ByteString.readFile path)
    payload <- case input of
      Left exception -> failText ("Cannot read statement import file: " <> Text.pack (show exception))
      Right bytes -> pure bytes
    importedAt <- getCurrentTime
    batch <- case decodeStatementImport payload importedAt of
      Left reason -> failText ("Invalid statement import file: " <> reason)
      Right value -> pure value
    storeRefreshBatch repository batch >>= reportStoreResult
    TIO.putStrLn
      ( "Imported "
          <> Text.pack (show (length (refreshCanonicalFacts batch)))
          <> " sourced facts for "
          <> unCompanyId (refreshCompany batch)
          <> "."
      )
  RefreshStatements symbol sources -> withRepository $ \repository -> do
    (providerName, batch, failedAttempts) <- fetchFromSources symbol sources []
    storeRefreshBatch repository batch >>= reportStoreResult
    TIO.putStrLn
      ( "Imported "
          <> Text.pack (show (length (refreshCanonicalFacts batch)))
          <> " "
          <> Text.toUpper providerName
          <> " sourced facts for "
          <> unCompanyId (refreshCompany batch)
          <> "."
      )
    TIO.putStrLn
      ( "Basis note: "
          <> Text.toUpper providerName
          <> " statement endpoints do not reliably label standalone versus consolidated; Seby records this data as consolidated by explicit adapter policy. Verify before relying on the basis."
      )
    mapM_ (TIO.putStrLn . ("Source attempt failed; continuing: " <>)) failedAttempts
  InvalidAnalysisRequest reason -> failText reason
  WatchlistAdd symbol -> withRepository $ \repository -> do
    addWatchlistSymbol repository symbol >>= reportStoreResult
    TIO.putStrLn ("Added " <> Text.toUpper (unSymbol symbol) <> " to the watchlist.")
  WatchlistRemove symbol -> withRepository $ \repository -> do
    removeWatchlistSymbol repository symbol >>= reportStoreResult
    TIO.putStrLn ("Removed " <> Text.toUpper (unSymbol symbol) <> " from the watchlist.")
  WatchlistList -> withRepository $ \repository -> do
    loadWatchlist repository >>= \case
      Left errorValue -> failStore errorValue
      Right symbols -> mapM_ (TIO.putStrLn . unSymbol) symbols
  DatabaseCheck -> withRepository $ \repository -> do
    checkDatabaseIntegrity repository >>= reportStoreResult
    TIO.putStrLn "SQLite integrity check passed."
  DatabaseVacuum -> withRepository $ \repository -> do
    vacuumDatabase repository >>= reportStoreResult
    TIO.putStrLn "SQLite database vacuum completed."
  DatabaseBackup path -> withRepository $ \repository -> do
    backupDatabase repository path >>= reportStoreResult
    TIO.putStrLn ("SQLite database backup written to " <> Text.pack path <> ".")
  PrintVersion -> do
    TIO.putStrLn "seby 0.1.0.0 — Indian Equities Fundamental Analysis System (Haskell GHC 9.14)"

withRepository :: (FactRepository -> IO a) -> IO a
withRepository action = do
  opened <- openFactRepository "seby.sqlite"
  case opened of
    Left errorValue -> failStore errorValue
    Right repository -> bracket (pure repository) closeFactRepository action

reportStoreResult :: Either StoreError () -> IO ()
reportStoreResult result = case result of
  Left errorValue -> failStore errorValue
  Right () -> pure ()

failStore :: StoreError -> IO a
failStore = die . Text.unpack . Text.pack . show

failText :: Text -> IO a
failText = die . Text.unpack

fetchFromSources :: Symbol -> [(Text, Symbol)] -> [Text] -> IO (Text, RefreshBatch, [Text])
fetchFromSources _ [] errors = failText ("All selected statement sources failed. " <> Text.intercalate "; " (reverse errors))
fetchFromSources company ((providerName, providerSymbol) : remaining) errors = do
  let normalizedProvider = Text.toLower providerName
  (keyVariable, fetcher) <- case normalizedProvider of
    "fmp" -> pure ("SEBY_FMP_API_KEY", fetchFmpStatements)
    "alpha-vantage" -> pure ("SEBY_ALPHA_VANTAGE_API_KEY", fetchAlphaVantageStatements)
    _ -> failText ("Unsupported statement provider in source order: " <> providerName)
  keyResult <- lookupEnv keyVariable
  case keyResult of
    Nothing -> attemptFailure (Text.pack keyVariable <> " is not set")
    Just key -> do
      fetched <- fetcher (Text.pack key) company providerSymbol
      case fetched of
        Left failure ->
          attemptFailure (providerName <> " (" <> unSymbol providerSymbol <> "): " <> fmpErrorText failure)
        Right batch -> pure (normalizedProvider, batch, reverse errors)
  where
    attemptFailure message = case remaining of
      [] ->
        failText
          ("All selected statement sources failed. " <> Text.intercalate "; " (reverse (message : errors)))
      _ -> fetchFromSources company remaining (message : errors)

fmpErrorText :: FmpError -> Text
fmpErrorText failure = case failure of
  FmpMissingApiKey -> "API key is missing"
  FmpInvalidProviderSymbol -> "FMP provider symbol contains unsupported characters"
  FmpNetworkFailure -> "network request failed or timed out"
  FmpHttpFailure status -> "provider returned HTTP " <> Text.pack (show status)
  FmpProviderMessage message -> "provider rejected the request: " <> message
  FmpInvalidPayload message -> "invalid provider response: " <> message
  FmpUnsupportedCurrency currency -> "unsupported reported currency: " <> currency
  FmpNoSupportedFacts message -> message
  FmpInvalidAlphaVantagePayload message -> "invalid Alpha Vantage response: " <> message
  FmpPayloadTooLarge limit ->
    "provider response exceeded the "
      <> Text.pack (show (limit `div` 1024 `div` 1024))
      <> " MiB safety limit"

loadReport :: FactRepository -> Symbol -> StatementBasis -> Text -> IO (Either Text Report)
loadReport repository symbol basis scope = do
  let company = CompanyId (unSymbol symbol)
  loaded <- loadCurrentCompanyFacts repository company
  case loaded of
    Left storeError -> pure (Left ("Could not load persisted facts: " <> Text.pack (show storeError)))
    Right facts -> do
      companyNameResult <- loadCompanyName repository company
      case companyNameResult of
        Left storeError -> pure (Left ("Could not load company identity: " <> Text.pack (show storeError)))
        Right Nothing ->
          pure
            ( Left
                ("No sourced company profile exists for " <> unSymbol symbol <> ". Import a statement file first.")
            )
        Right (Just name) -> do
          today <- utctDay <$> getCurrentTime
          pure (buildCompanyReport today company symbol name basis scope facts)

emitBatchResult :: (Symbol, Either Text Report) -> IO ()
emitBatchResult (symbol, result) = case result of
  Left reason -> TIO.hPutStrLn stderr ("[FAILED] " <> unSymbol symbol <> ": " <> reason)
  Right report -> do
    TIO.putStr (renderTerminal report)
    TIO.putStrLn ""

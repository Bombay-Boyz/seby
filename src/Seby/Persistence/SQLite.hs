{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | SQLite implementation of the fact repository. Transactions retain the
prior committed snapshot whenever a refresh batch fails.
-}
module Seby.Persistence.SQLite (
  openFactRepository,
  currentSchemaVersion,
  maxPayloadBytes,
  payloadSha256,
) where

import Control.Exception (bracketOnError, displayException, try)
import Control.Monad (forM_, when)
import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Char (isSpace)
import Data.Ratio (Ratio, denominator, numerator, (%))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, UTCTime, defaultTimeLocale, formatTime, getCurrentTime, parseTimeM)
import Database.SQLite.Simple
import Seby.Analysis.CorporateActions
import Seby.Domain.Money (Currency (..))
import Seby.Domain.Types (
  CompanyId (..),
  PeriodType (..),
  Provenance (..),
  SourceId (..),
  StatementBasis (..),
  Symbol (..),
  unCompanyId,
 )
import Seby.Persistence.Repository (FactRepository (..))
import Seby.Persistence.Types

currentSchemaVersion :: Int
currentSchemaVersion = 2

-- | Upper bound for a source payload persisted by a single refresh batch.
maxPayloadBytes :: Int
maxPayloadBytes = 64 * 1024 * 1024

data StoredFact
  = StoredFact
      !Text
      !Text
      !Text
      !Text
      !Text
      !Text
      !Text
      !Text
      !Text
      !Text
      !(Maybe Text)
      !Text
      !(Maybe Text)
      !Text

data StoredAction
  = StoredAction !Text !Text !Text !Text !Text !Text !(Maybe Text) !Text !(Maybe Text) !Text

data CanonicalInsert
  = CanonicalInsert
      !Text
      !Text
      !Text
      !Text
      !Text
      !Text
      !Text
      !Text
      !Text
      !Text
      !Text
      !Text
      !Text
      !(Maybe Text)
      !(Maybe Text)

instance ToRow CanonicalInsert where
  toRow (CanonicalInsert a b c d e f g h i j k l m n o) =
    toRow (a, b, c, d, e, f, g, h, i, j) <> toRow (k, l, m, n, o)

instance FromRow StoredFact where
  fromRow =
    StoredFact
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

instance FromRow StoredAction where
  fromRow =
    StoredAction
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

openFactRepository :: FilePath -> IO (Either StoreError FactRepository)
openFactRepository path = do
  opened <- try @SQLError $ do
    connection <- open path
    bracketOnError (pure connection) close $ \conn -> do
      configureConnection conn
      migration <- migrate conn
      case migration of
        Left storeError -> close conn >> pure (Left storeError)
        Right () -> pure (Right conn)
  case opened of
    Left err -> pure (Left (DatabaseFailure (Text.pack (displayException err))))
    Right (Left storeError) -> pure (Left storeError)
    Right (Right connection) ->
      pure . Right $
        FactRepository
          { closeFactRepository = close connection
          , storeRefreshBatch = storeBatch connection
          , recordRefreshFailure = recordFailure connection
          , loadCanonicalFacts = loadFacts connection
          , loadCanonicalFactHistory = loadFactHistory connection
          , loadCurrentCompanyFacts = loadCurrentFacts connection
          , loadCompanyName = getCompanyName connection
          , persistCorporateActions = saveActions connection
          , loadCorporateActions = loadActions connection
          , addWatchlistSymbol = addWatchlist connection
          , removeWatchlistSymbol = removeWatchlist connection
          , loadWatchlist = loadWatchlistSymbols connection
          , checkDatabaseIntegrity = checkIntegrity connection
          , vacuumDatabase = vacuum connection
          , getRefreshState = refreshState connection
          }

configureConnection :: Connection -> IO ()
configureConnection conn = do
  -- WAL permits readers alongside the single writer; FULL keeps durable commits.
  execute_ conn "PRAGMA foreign_keys = ON"
  execute_ conn "PRAGMA busy_timeout = 5000"
  execute_ conn "PRAGMA synchronous = FULL"
  _ <- query_ conn "PRAGMA journal_mode = WAL" :: IO [Only Text]
  pure ()

migrate :: Connection -> IO (Either StoreError ())
migrate conn = do
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS schema_migrations (version INTEGER PRIMARY KEY, applied_at TEXT NOT NULL)"
  versionRows <-
    query_ conn "SELECT COALESCE(MAX(version), 0) FROM schema_migrations" :: IO [Only Int]
  let maxVersion = case versionRows of
        Only version : _ -> version
        [] -> 0
  if maxVersion > currentSchemaVersion
    then pure (Left (UnsupportedSchemaVersion maxVersion))
    else do
      when (maxVersion < 1) $ withTransaction conn $ do
        mapM_ (execute_ conn) schemaV1
        now <- formatUtc <$> getCurrentTime
        execute conn "INSERT INTO schema_migrations(version, applied_at) VALUES (1, ?)" (Only now)
      when (maxVersion < 2) $ withTransaction conn $ do
        mapM_ (execute_ conn) schemaV2
        now <- formatUtc <$> getCurrentTime
        execute conn "INSERT INTO schema_migrations(version, applied_at) VALUES (2, ?)" (Only now)
      pure (Right ())

schemaV2 :: [Query]
schemaV2 =
  [ "CREATE TABLE watchlist (symbol TEXT PRIMARY KEY CHECK(length(trim(symbol)) > 0), added_at TEXT NOT NULL)"
  ]

schemaV1 :: [Query]
schemaV1 =
  [ "CREATE TABLE source (source_id TEXT PRIMARY KEY CHECK(length(trim(source_id)) > 0), name TEXT NOT NULL CHECK(length(trim(name)) > 0))"
  , "CREATE TABLE company (company_id TEXT PRIMARY KEY CHECK(length(trim(company_id)) > 0), name TEXT NOT NULL DEFAULT '')"
  , "CREATE TABLE security (security_id TEXT PRIMARY KEY, company_id TEXT NOT NULL REFERENCES company(company_id), exchange TEXT NOT NULL, symbol TEXT NOT NULL, UNIQUE(exchange, symbol))"
  , "CREATE TABLE identifier_mapping (mapping_id INTEGER PRIMARY KEY, company_id TEXT NOT NULL REFERENCES company(company_id), namespace TEXT NOT NULL, identifier TEXT NOT NULL, effective_from TEXT NOT NULL, effective_to TEXT, CHECK(effective_to IS NULL OR effective_to >= effective_from), UNIQUE(namespace, identifier, effective_from))"
  , "CREATE TABLE filing (filing_id TEXT PRIMARY KEY, company_id TEXT NOT NULL REFERENCES company(company_id), source_id TEXT NOT NULL, payload_digest TEXT NOT NULL, uri TEXT, published_at TEXT, period_start TEXT, period_end TEXT, CHECK(period_start IS NULL OR period_end IS NULL OR period_end >= period_start), FOREIGN KEY(source_id, payload_digest) REFERENCES raw_payload(source_id, digest))"
  , "CREATE TABLE raw_payload (source_id TEXT NOT NULL REFERENCES source(source_id), digest TEXT NOT NULL CHECK(length(digest) = 64), uri TEXT, retrieved_at TEXT NOT NULL, published_at TEXT, body BLOB NOT NULL CHECK(length(body) <= 67108864), PRIMARY KEY(source_id, digest))"
  , "CREATE TABLE raw_fact (raw_fact_id INTEGER PRIMARY KEY, source_id TEXT NOT NULL, payload_digest TEXT NOT NULL, extractor_version TEXT NOT NULL CHECK(length(trim(extractor_version)) > 0), ordinal INTEGER NOT NULL CHECK(ordinal >= 0), raw_name TEXT NOT NULL, raw_value TEXT NOT NULL, raw_unit TEXT NOT NULL, period_start TEXT NOT NULL, period_end TEXT NOT NULL CHECK(period_end >= period_start), FOREIGN KEY(source_id, payload_digest) REFERENCES raw_payload(source_id, digest), UNIQUE(source_id, payload_digest, extractor_version, ordinal))"
  , "CREATE TABLE canonical_fact (fact_id INTEGER PRIMARY KEY, company_id TEXT NOT NULL REFERENCES company(company_id), source_id TEXT NOT NULL, payload_digest TEXT NOT NULL, concept TEXT NOT NULL CHECK(length(trim(concept)) > 0), period_start TEXT NOT NULL, period_end TEXT NOT NULL CHECK(period_end >= period_start), period_type TEXT NOT NULL CHECK(period_type IN ('annual', 'quarterly', 'ttm')), currency TEXT NOT NULL CHECK(currency IN ('INR', 'USD', 'EUR')), amount_minor TEXT NOT NULL, statement_basis TEXT NOT NULL CHECK(statement_basis IN ('standalone', 'consolidated')), scope TEXT NOT NULL, policy_version TEXT NOT NULL CHECK(length(trim(policy_version)) > 0), observed_at TEXT NOT NULL, published_at TEXT, provenance_uri TEXT, FOREIGN KEY(source_id, payload_digest) REFERENCES raw_payload(source_id, digest), UNIQUE(company_id, concept, period_start, period_end, period_type, currency, statement_basis, scope, policy_version, source_id, payload_digest))"
  , "CREATE INDEX canonical_fact_lookup ON canonical_fact(company_id, period_start, period_end, period_type)"
  , "CREATE TABLE corporate_action (action_id TEXT PRIMARY KEY, company_id TEXT NOT NULL REFERENCES company(company_id), action_type TEXT NOT NULL, effective_date TEXT NOT NULL, source_id TEXT NOT NULL, payload_digest TEXT NOT NULL, details TEXT NOT NULL, FOREIGN KEY(source_id, payload_digest) REFERENCES raw_payload(source_id, digest))"
  , "CREATE TABLE metric_definition (metric_id TEXT NOT NULL, formula_version TEXT NOT NULL, description TEXT NOT NULL, PRIMARY KEY(metric_id, formula_version))"
  , "CREATE TABLE derived_metric (derived_metric_id INTEGER PRIMARY KEY, company_id TEXT NOT NULL REFERENCES company(company_id), metric_id TEXT NOT NULL, formula_version TEXT NOT NULL, period_start TEXT NOT NULL, period_end TEXT NOT NULL, value TEXT NOT NULL, input_manifest TEXT NOT NULL, FOREIGN KEY(metric_id, formula_version) REFERENCES metric_definition(metric_id, formula_version))"
  , "CREATE TABLE refresh_run (run_id TEXT PRIMARY KEY, company_id TEXT NOT NULL REFERENCES company(company_id), source_id TEXT NOT NULL REFERENCES source(source_id), payload_digest TEXT, attempted_at TEXT NOT NULL, completed_at TEXT, status TEXT NOT NULL CHECK(status IN ('succeeded', 'failed')), error TEXT, fact_count INTEGER NOT NULL DEFAULT 0 CHECK(fact_count >= 0))"
  , "CREATE TABLE report_snapshot (snapshot_id TEXT PRIMARY KEY, company_id TEXT NOT NULL REFERENCES company(company_id), created_at TEXT NOT NULL, formula_version TEXT NOT NULL, input_manifest TEXT NOT NULL, rendered_report TEXT NOT NULL)"
  ]

storeBatch :: Connection -> RefreshBatch -> IO (Either StoreError ())
storeBatch conn batch = do
  let payload = refreshPayload batch
      provenance = rawPayloadProvenance payload
      bytes = rawPayloadBytes payload
      size = ByteString.length bytes
  if size > maxPayloadBytes
    then pure (Left (PayloadTooLarge (toInteger size) (toInteger maxPayloadBytes)))
    else
      let actualDigest = payloadSha256 bytes
          expectedDigest = provenanceDigest provenance
       in if actualDigest /= expectedDigest
            then pure (Left (InvalidPayloadDigest expectedDigest actualDigest))
            else case validateBatch batch of
              Just reason -> pure (Left (InvalidRefreshBatch reason))
              Nothing -> persistBatch conn batch provenance actualDigest

persistBatch :: Connection -> RefreshBatch -> Provenance -> Text -> IO (Either StoreError ())
persistBatch conn batch provenance actualDigest = do
  result <- try @SQLError $ withTransaction conn $ do
    let source = unSourceId (provenanceSource provenance)
        company = unCompanyId (refreshCompany batch)
        digest = Text.toLower actualDigest
    execute
      conn
      "INSERT INTO source(source_id, name) VALUES (?, ?) ON CONFLICT(source_id) DO NOTHING"
      (source, source)
    execute
      conn
      "INSERT INTO company(company_id, name) VALUES (?, ?) ON CONFLICT(company_id) DO UPDATE SET name = excluded.name WHERE length(trim(excluded.name)) > 0"
      (company, refreshCompanyName batch)
    execute
      conn
      "INSERT INTO raw_payload(source_id, digest, uri, retrieved_at, published_at, body) VALUES (?, ?, ?, ?, ?, ?) ON CONFLICT(source_id, digest) DO NOTHING"
      ( source
      , digest
      , provenanceUri provenance
      , formatUtc (provenanceRetrievedAt provenance)
      , formatUtc <$> provenancePublishedAt provenance
      , rawPayloadBytes (refreshPayload batch)
      )
    execute
      conn
      "INSERT INTO refresh_run(run_id, company_id, source_id, payload_digest, attempted_at, completed_at, status, fact_count) VALUES (?, ?, ?, ?, ?, ?, 'succeeded', ?)"
      ( refreshRunId batch
      , company
      , source
      , digest
      , formatUtc (refreshAttemptedAt batch)
      , formatUtc (refreshCompletedAt batch)
      , length (refreshCanonicalFacts batch)
      )
    forM_ (zip [0 :: Int ..] (refreshRawObservations batch)) $ \(ordinal, observation) ->
      execute
        conn
        "INSERT INTO raw_fact(source_id, payload_digest, extractor_version, ordinal, raw_name, raw_value, raw_unit, period_start, period_end) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT(source_id, payload_digest, extractor_version, ordinal) DO NOTHING"
        ( source
        , digest
        , rawObservationExtractorVersion observation
        , ordinal
        , rawObservationName observation
        , rawObservationValue observation
        , rawObservationUnit observation
        , formatDay (rawObservationPeriodStart observation)
        , formatDay (rawObservationPeriodEnd observation)
        )
    forM_ (refreshCanonicalFacts batch) $ \fact ->
      execute
        conn
        "INSERT INTO canonical_fact(company_id, source_id, payload_digest, concept, period_start, period_end, period_type, currency, amount_minor, statement_basis, scope, policy_version, observed_at, published_at, provenance_uri) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT(company_id, concept, period_start, period_end, period_type, currency, statement_basis, scope, policy_version, source_id, payload_digest) DO NOTHING"
        ( CanonicalInsert
            company
            source
            digest
            (canonicalConcept fact)
            (formatDay (canonicalPeriodStart fact))
            (formatDay (canonicalPeriodEnd fact))
            (periodTypeText (canonicalPeriodType fact))
            (currencyText (canonicalCurrency fact))
            (Text.pack (show (canonicalAmountMinor fact)))
            (basisText (canonicalStatementBasis fact))
            (canonicalScope fact)
            (canonicalPolicyVersion fact)
            (formatUtc (provenanceRetrievedAt (canonicalProvenance fact)))
            (formatUtc <$> provenancePublishedAt (canonicalProvenance fact))
            (provenanceUri (canonicalProvenance fact))
        )
  case result of
    Right () -> pure (Right ())
    Left exception -> do
      let originalError = Text.pack (displayException exception)
          failure =
            RefreshFailure
              (refreshRunId batch)
              (refreshCompany batch)
              (provenanceSource provenance)
              (refreshAttemptedAt batch)
              originalError
      persistedFailure <- recordFailure conn failure
      pure $ case persistedFailure of
        Left persistenceError ->
          Left
            ( DatabaseFailure
                (originalError <> "; also failed to record refresh failure: " <> Text.pack (show persistenceError))
            )
        Right () -> Left (DatabaseFailure originalError)

validateBatch :: RefreshBatch -> Maybe Text
validateBatch batch
  | Text.null (Text.strip (refreshRunId batch)) = Just "Refresh run ID cannot be empty"
  | Text.null (Text.strip (unCompanyId (refreshCompany batch))) = Just "Company ID cannot be empty"
  | Text.null (Text.strip (unSourceId (provenanceSource payloadProvenance))) =
      Just "Source ID cannot be empty"
  | any invalidRaw (refreshRawObservations batch) =
      Just "Raw observation has an empty name or unit, or an invalid period"
  | any invalidCanonical (refreshCanonicalFacts batch) =
      Just "Canonical fact is invalid or belongs to another company/source"
  | otherwise = Nothing
  where
    payloadProvenance = rawPayloadProvenance (refreshPayload batch)
    invalidRaw observation =
      Text.null (Text.strip (rawObservationName observation))
        || Text.null (Text.strip (rawObservationUnit observation))
        || Text.null (Text.strip (rawObservationExtractorVersion observation))
        || rawObservationPeriodEnd observation < rawObservationPeriodStart observation
    invalidCanonical fact =
      canonicalCompany fact /= refreshCompany batch
        || canonicalProvenance fact /= rawPayloadProvenance (refreshPayload batch)
        || Text.null (Text.strip (canonicalConcept fact))
        || Text.null (Text.strip (canonicalScope fact))
        || Text.null (Text.strip (canonicalPolicyVersion fact))
        || canonicalPeriodEnd fact < canonicalPeriodStart fact

recordFailure :: Connection -> RefreshFailure -> IO (Either StoreError ())
recordFailure conn failure
  | Text.null (Text.strip (failedRunId failure)) =
      pure (Left (InvalidRefreshBatch "Refresh run ID cannot be empty"))
  | Text.null (Text.strip (unCompanyId (failedCompany failure))) =
      pure (Left (InvalidRefreshBatch "Company ID cannot be empty"))
  | Text.null (Text.strip (unSourceId (failedSource failure))) =
      pure (Left (InvalidRefreshBatch "Source ID cannot be empty"))
  | otherwise = runDatabase $ withTransaction conn $ do
      let source = unSourceId (failedSource failure)
          company = unCompanyId (failedCompany failure)
      execute
        conn
        "INSERT INTO source(source_id, name) VALUES (?, ?) ON CONFLICT(source_id) DO NOTHING"
        (source, source)
      execute
        conn
        "INSERT INTO company(company_id) VALUES (?) ON CONFLICT(company_id) DO NOTHING"
        (Only company)
      execute
        conn
        "INSERT INTO refresh_run(run_id, company_id, source_id, attempted_at, status, error) VALUES (?, ?, ?, ?, 'failed', ?)"
        (failedRunId failure, company, source, formatUtc (failedAt failure), failureReason failure)
      pure (Right ())

addWatchlist :: Connection -> Symbol -> IO (Either StoreError ())
addWatchlist conn (Symbol rawSymbol) = case normalizeWatchlistSymbol rawSymbol of
  Left errorValue -> pure (Left errorValue)
  Right symbol -> runDatabase $ do
    now <- formatUtc <$> getCurrentTime
    execute
      conn
      "INSERT INTO watchlist(symbol, added_at) VALUES (?, ?) ON CONFLICT(symbol) DO NOTHING"
      (symbol, now)
    pure (Right ())

removeWatchlist :: Connection -> Symbol -> IO (Either StoreError ())
removeWatchlist conn (Symbol rawSymbol) = case normalizeWatchlistSymbol rawSymbol of
  Left errorValue -> pure (Left errorValue)
  Right symbol -> runDatabase $ do
    execute conn "DELETE FROM watchlist WHERE symbol = ?" (Only symbol)
    pure (Right ())

loadWatchlistSymbols :: Connection -> IO (Either StoreError [Symbol])
loadWatchlistSymbols conn = runDatabase $ do
  rows <- query_ conn "SELECT symbol FROM watchlist ORDER BY symbol" :: IO [Only Text]
  pure (Right [Symbol symbol | Only symbol <- rows])

normalizeWatchlistSymbol :: Text -> Either StoreError Text
normalizeWatchlistSymbol rawSymbol
  | Text.null normalized = Left (InvalidWatchlistSymbol rawSymbol)
  | Text.any isSpace normalized = Left (InvalidWatchlistSymbol rawSymbol)
  | otherwise = Right normalized
  where
    normalized = Text.toUpper (Text.strip rawSymbol)

checkIntegrity :: Connection -> IO (Either StoreError ())
checkIntegrity conn = runDatabase $ do
  rows <- query_ conn "PRAGMA integrity_check" :: IO [Only Text]
  pure $ if rows == [Only "ok"] then Right () else Left (DatabaseFailure (Text.pack (show rows)))

vacuum :: Connection -> IO (Either StoreError ())
vacuum conn = runDatabase $ do
  execute_ conn "VACUUM"
  pure (Right ())

loadFacts :: Connection -> CompanyId -> FactPeriod -> IO (Either StoreError [CanonicalFact])
loadFacts conn (CompanyId company) period = runDatabase $ do
  rows <-
    query
      conn
      "SELECT f.concept, f.period_start, f.period_end, f.period_type, f.currency, f.amount_minor, f.statement_basis, f.scope, f.policy_version, p.source_id, f.provenance_uri, f.observed_at, f.published_at, f.payload_digest FROM canonical_fact f JOIN raw_payload p ON p.source_id = f.source_id AND p.digest = f.payload_digest WHERE f.company_id = ? AND f.period_start = ? AND f.period_end = ? AND f.period_type = ? AND f.fact_id = (SELECT f2.fact_id FROM canonical_fact f2 WHERE f2.company_id = f.company_id AND f2.concept = f.concept AND f2.period_start = f.period_start AND f2.period_end = f.period_end AND f2.period_type = f.period_type AND f2.currency = f.currency AND f2.statement_basis = f.statement_basis AND f2.scope = f.scope AND f2.source_id = f.source_id ORDER BY julianday(COALESCE(f2.published_at, f2.observed_at)) DESC, f2.fact_id DESC LIMIT 1) ORDER BY f.concept, f.statement_basis, f.scope, p.source_id"
      ( company
      , formatDay (factPeriodStart period)
      , formatDay (factPeriodEnd period)
      , periodTypeText (factPeriodType period)
      ) ::
      IO [StoredFact]
  pure (traverse (decodeStoredFact company) rows)

loadFactHistory :: Connection -> CompanyId -> FactPeriod -> IO (Either StoreError [CanonicalFact])
loadFactHistory conn (CompanyId company) period = runDatabase $ do
  rows <-
    query
      conn
      "SELECT f.concept, f.period_start, f.period_end, f.period_type, f.currency, f.amount_minor, f.statement_basis, f.scope, f.policy_version, p.source_id, f.provenance_uri, f.observed_at, f.published_at, f.payload_digest FROM canonical_fact f JOIN raw_payload p ON p.source_id = f.source_id AND p.digest = f.payload_digest WHERE f.company_id = ? AND f.period_start = ? AND f.period_end = ? AND f.period_type = ? ORDER BY f.concept, f.statement_basis, f.scope, p.source_id, julianday(COALESCE(f.published_at, f.observed_at)), f.fact_id"
      ( company
      , formatDay (factPeriodStart period)
      , formatDay (factPeriodEnd period)
      , periodTypeText (factPeriodType period)
      ) ::
      IO [StoredFact]
  pure (traverse (decodeStoredFact company) rows)

loadCurrentFacts :: Connection -> CompanyId -> IO (Either StoreError [CanonicalFact])
loadCurrentFacts conn (CompanyId company) = runDatabase $ do
  rows <-
    query
      conn
      "SELECT f.concept, f.period_start, f.period_end, f.period_type, f.currency, f.amount_minor, f.statement_basis, f.scope, f.policy_version, p.source_id, f.provenance_uri, f.observed_at, f.published_at, f.payload_digest FROM canonical_fact f JOIN raw_payload p ON p.source_id = f.source_id AND p.digest = f.payload_digest WHERE f.company_id = ? AND f.fact_id = (SELECT f2.fact_id FROM canonical_fact f2 WHERE f2.company_id = f.company_id AND f2.concept = f.concept AND f2.period_start = f.period_start AND f2.period_end = f.period_end AND f2.period_type = f.period_type AND f2.currency = f.currency AND f2.statement_basis = f.statement_basis AND f2.scope = f.scope AND f2.source_id = f.source_id ORDER BY julianday(COALESCE(f2.published_at, f2.observed_at)) DESC, f2.fact_id DESC LIMIT 1) ORDER BY f.concept, f.period_end, f.statement_basis, f.scope, p.source_id"
      (Only company) ::
      IO [StoredFact]
  pure (traverse (decodeStoredFact company) rows)

getCompanyName :: Connection -> CompanyId -> IO (Either StoreError (Maybe Text))
getCompanyName conn (CompanyId company) = runDatabase $ do
  rows <- query conn "SELECT name FROM company WHERE company_id = ?" (Only company) :: IO [Only Text]
  pure $ case rows of
    [] -> Right Nothing
    Only name : _ -> Right (if Text.null (Text.strip name) then Nothing else Just name)

saveActions :: Connection -> [CorporateAction] -> IO (Either StoreError ())
saveActions conn actions
  | any invalidAction actions =
      pure (Left (InvalidRefreshBatch "Corporate action has an empty ID or company ID"))
  | otherwise = runDatabase $ withTransaction conn $ do
      forM_ actions $ \action -> do
        let company = unCompanyId (corporateActionCompany action)
            provenance = corporateActionProvenance action
            source = unSourceId (provenanceSource provenance)
        execute
          conn
          "INSERT INTO company(company_id) VALUES (?) ON CONFLICT(company_id) DO NOTHING"
          (Only company)
        execute
          conn
          "INSERT INTO corporate_action(action_id, company_id, action_type, effective_date, source_id, payload_digest, details) VALUES (?, ?, ?, ?, ?, ?, ?) ON CONFLICT(action_id) DO NOTHING"
          ( corporateActionId action
          , company
          , actionTypeText (corporateActionType action)
          , formatDay (corporateActionEffectiveDate action)
          , source
          , Text.toLower (provenanceDigest provenance)
          , actionDetails (corporateActionType action)
          )
      pure (Right ())
  where
    invalidAction action =
      Text.null (Text.strip (corporateActionId action))
        || Text.null (Text.strip (unCompanyId (corporateActionCompany action)))

loadActions :: Connection -> CompanyId -> IO (Either StoreError [CorporateAction])
loadActions conn (CompanyId company) = runDatabase $ do
  rows <-
    query
      conn
      "SELECT a.action_id, a.company_id, a.action_type, a.effective_date, a.details, a.source_id, p.uri, p.retrieved_at, p.published_at, a.payload_digest FROM corporate_action a JOIN raw_payload p ON p.source_id = a.source_id AND p.digest = a.payload_digest WHERE a.company_id = ? ORDER BY a.effective_date, a.action_id"
      (Only company) ::
      IO [StoredAction]
  pure (traverse decodeStoredAction rows)

decodeStoredAction :: StoredAction -> Either StoreError CorporateAction
decodeStoredAction
  ( StoredAction
      identifier
      company
      actionType
      effectiveDate
      details
      source
      uri
      retrieved
      published
      digest
    ) = do
    day <- parseDay effectiveDate
    retrievedAt <- parseUtc retrieved
    publishedAt <- traverse parseUtc published
    value <- parseActionType actionType details
    let provenance = Provenance (SourceId source) uri retrievedAt publishedAt digest
    case mkCorporateAction identifier (CompanyId company) day value provenance of
      Left errorValue -> Left (InvalidStoredValue (Text.pack (show errorValue)))
      Right action -> Right action

actionTypeText :: CorporateActionType -> Text
actionTypeText actionType = case actionType of
  ShareSplit _ -> "share_split"
  BonusIssue _ -> "bonus_issue"
  RightsIssue -> "rights_issue"
  Merger -> "merger"
  Demerger -> "demerger"
  ConsolidationScopeChange -> "consolidation_scope_change"
  ShareIssuance -> "share_issuance"
  Buyback -> "buyback"

actionDetails :: CorporateActionType -> Text
actionDetails actionType = case actionType of
  ShareSplit ratio -> ratioText ratio
  BonusIssue ratio -> ratioText ratio
  _ -> ""

ratioText :: Ratio Integer -> Text
ratioText ratio = Text.pack (show (numerator ratio)) <> ":" <> Text.pack (show (denominator ratio))

parseActionType :: Text -> Text -> Either StoreError CorporateActionType
parseActionType actionType details = case actionType of
  "share_split" -> ShareSplit <$> parseRatio details
  "bonus_issue" -> BonusIssue <$> parseRatio details
  "rights_issue" -> Right RightsIssue
  "merger" -> Right Merger
  "demerger" -> Right Demerger
  "consolidation_scope_change" -> Right ConsolidationScopeChange
  "share_issuance" -> Right ShareIssuance
  "buyback" -> Right Buyback
  _ -> Left (InvalidStoredValue ("Invalid corporate action type: " <> actionType))

parseRatio :: Text -> Either StoreError (Ratio Integer)
parseRatio value = case Text.splitOn ":" value of
  [rawNumerator, rawDenominator] -> do
    numeratorValue <- parseInteger rawNumerator
    denominatorValue <- parseInteger rawDenominator
    if denominatorValue > 0 && numeratorValue > 0
      then Right (numeratorValue % denominatorValue)
      else Left (InvalidStoredValue ("Invalid corporate action ratio: " <> value))
  _ -> Left (InvalidStoredValue ("Invalid corporate action ratio: " <> value))

parseInteger :: Text -> Either StoreError Integer
parseInteger value = case reads (Text.unpack value) of
  [(number, "")] -> Right number
  _ -> Left (InvalidStoredValue ("Invalid integer: " <> value))

refreshState :: Connection -> CompanyId -> IO (Either StoreError (Maybe RefreshState))
refreshState conn (CompanyId company) = runDatabase $ do
  rows <-
    query
      conn
      "SELECT latest.attempted_at, (SELECT completed_at FROM refresh_run WHERE company_id = ? AND status = 'succeeded' ORDER BY julianday(completed_at) DESC, run_id DESC LIMIT 1), latest.error FROM (SELECT attempted_at, error FROM refresh_run WHERE company_id = ? ORDER BY julianday(attempted_at) DESC, run_id DESC LIMIT 1) latest"
      (company, company) ::
      IO [(Text, Maybe Text, Maybe Text)]
  pure $ case rows of
    [] -> Right Nothing
    (attempted, completed, failure) : _ -> do
      attemptedAt <- parseUtc attempted
      successfulAt <- traverse parseUtc completed
      Right (Just (RefreshState attemptedAt successfulAt failure))

decodeStoredFact :: Text -> StoredFact -> Either StoreError CanonicalFact
decodeStoredFact
  company
  ( StoredFact
      concept
      start
      end
      period
      currency
      amount
      basis
      scope
      policyVersion
      source
      uri
      retrieved
      published
      digest
    ) = do
    periodStart <- parseDay start
    periodEnd <- parseDay end
    periodType <- parsePeriodType period
    currencyValue <- parseCurrency currency
    amountValue <-
      maybe (Left (InvalidStoredValue ("Invalid amount: " <> amount))) Right (readMaybeInteger amount)
    basisValue <- parseBasis basis
    retrievedAt <- parseUtc retrieved
    publishedAt <- traverse parseUtc published
    pure
      CanonicalFact
        { canonicalCompany = CompanyId company
        , canonicalConcept = concept
        , canonicalPeriodStart = periodStart
        , canonicalPeriodEnd = periodEnd
        , canonicalPeriodType = periodType
        , canonicalCurrency = currencyValue
        , canonicalAmountMinor = amountValue
        , canonicalStatementBasis = basisValue
        , canonicalScope = scope
        , canonicalPolicyVersion = policyVersion
        , canonicalProvenance = Provenance (SourceId source) uri retrievedAt publishedAt digest
        }

runDatabase :: IO (Either StoreError a) -> IO (Either StoreError a)
runDatabase action = do
  result <- try @SQLError action
  pure $ case result of
    Left err -> Left (DatabaseFailure (Text.pack (displayException err)))
    Right value -> value

payloadSha256 :: ByteString -> Text
payloadSha256 bytes = Text.pack (show (hash bytes :: Digest SHA256))

formatDay :: Day -> Text
formatDay day = Text.pack (formatTime defaultTimeLocale "%F" day)

parseDay :: Text -> Either StoreError Day
parseDay value =
  maybe
    (Left (InvalidStoredValue ("Invalid date: " <> value)))
    Right
    (parseTimeM True defaultTimeLocale "%F" (Text.unpack value))

formatUtc :: UTCTime -> Text
formatUtc = Text.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"

parseUtc :: Text -> Either StoreError UTCTime
parseUtc value =
  maybe
    (Left (InvalidStoredValue ("Invalid timestamp: " <> value)))
    Right
    (parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (Text.unpack value))

readMaybeInteger :: Text -> Maybe Integer
readMaybeInteger value = case reads (Text.unpack value) of
  [(number, "")] -> Just number
  _ -> Nothing

parsePeriodType :: Text -> Either StoreError PeriodType
parsePeriodType "annual" = Right Annual
parsePeriodType "quarterly" = Right Quarterly
parsePeriodType "ttm" = Right TrailingTwelveMonths
parsePeriodType value = Left (InvalidStoredValue ("Invalid period type: " <> value))

periodTypeText :: PeriodType -> Text
periodTypeText Annual = "annual"
periodTypeText Quarterly = "quarterly"
periodTypeText TrailingTwelveMonths = "ttm"

parseCurrency :: Text -> Either StoreError Currency
parseCurrency "INR" = Right INR
parseCurrency "USD" = Right USD
parseCurrency "EUR" = Right EUR
parseCurrency value = Left (InvalidStoredValue ("Invalid currency: " <> value))

currencyText :: Currency -> Text
currencyText INR = "INR"
currencyText USD = "USD"
currencyText EUR = "EUR"

parseBasis :: Text -> Either StoreError StatementBasis
parseBasis "standalone" = Right Standalone
parseBasis "consolidated" = Right Consolidated
parseBasis value = Left (InvalidStoredValue ("Invalid statement basis: " <> value))

basisText :: StatementBasis -> Text
basisText Standalone = "standalone"
basisText Consolidated = "consolidated"

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Strict import format for analyst-transcribed facts from a cited filing.
This is an offline workflow: it never downloads or scrapes the cited document.
-}
module Seby.Ingestion.LocalStatements (
  decodeStatementImport,
  maxImportedFacts,
) where

import Data.Aeson (FromJSON (..), eitherDecodeStrict', withObject, (.:))
import Data.Aeson.Types qualified as AesonTypes
import Data.ByteString (ByteString)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, UTCTime, defaultTimeLocale, formatTime, parseTimeM)
import Seby.Domain.Money (Currency (..))
import Seby.Domain.Types (
  CompanyId (..),
  PeriodType (..),
  Provenance (..),
  SourceId (..),
  StatementBasis (..),
 )
import Seby.Persistence.SQLite (payloadSha256)
import Seby.Persistence.Types

maxImportedFacts :: Int
maxImportedFacts = 10000

data StatementDocument = StatementDocument !Text !Text !Text !Text !UTCTime ![ImportedFact]

data ImportedFact = ImportedFact !Text !Day !Day !PeriodType !Currency !Integer !StatementBasis !Text

instance FromJSON StatementDocument where
  parseJSON = withObject "statement import" $ \fields -> do
    symbol <- fields .: "symbol"
    name <- fields .: "company_name"
    source <- fields .: "source_id"
    sourceUri <- fields .: "source_uri"
    publishedText <- fields .: "published_at"
    publishedAt <- parseTimestamp "published_at" publishedText
    facts <- fields .: "facts"
    pure (StatementDocument symbol name source sourceUri publishedAt facts)

instance FromJSON ImportedFact where
  parseJSON = withObject "financial fact" $ \fields -> do
    concept <- fields .: "concept"
    startText <- fields .: "period_start"
    endText <- fields .: "period_end"
    start <- parseDate "period_start" startText
    end <- parseDate "period_end" endText
    period <- fields .: "period_type" >>= parsePeriodType
    currency <- fields .: "currency" >>= parseCurrency
    amount <- fields .: "amount_minor"
    basis <- fields .: "basis" >>= parseBasis
    scope <- fields .: "scope"
    pure (ImportedFact concept start end period currency amount basis scope)

decodeStatementImport :: ByteString -> UTCTime -> Either Text RefreshBatch
decodeStatementImport payload importedAt = do
  value <- case eitherDecodeStrict' payload of
    Left message -> Left (Text.pack message)
    Right parsed -> Right parsed
  document <- case AesonTypes.parseEither parseJSON value of
    Left message -> Left (Text.pack message)
    Right parsed -> Right parsed
  buildBatch payload importedAt document

buildBatch :: ByteString -> UTCTime -> StatementDocument -> Either Text RefreshBatch
buildBatch payload importedAt (StatementDocument rawSymbol rawName rawSource sourceUri publishedAt facts) = do
  symbol <- validateSymbol rawSymbol
  name <- requireText "company_name" rawName
  source <- requireText "source_id" rawSource
  uri <- validateSourceUri sourceUri
  validateFacts facts
  let digest = payloadSha256 payload
      provenance = Provenance (SourceId source) (Just uri) importedAt (Just publishedAt) digest
      company = CompanyId symbol
      canonical = fmap (toCanonical company provenance) facts
      observations = fmap toRaw facts
      runIdentifier =
        "local-import:"
          <> digest
          <> ":"
          <> Text.pack (formatTime defaultTimeLocale "%Y%m%dT%H%M%S%qZ" importedAt)
  pure
    RefreshBatch
      { refreshRunId = runIdentifier
      , refreshCompany = company
      , refreshCompanyName = name
      , refreshAttemptedAt = importedAt
      , refreshCompletedAt = importedAt
      , refreshPayload = RawPayload provenance payload
      , refreshRawObservations = observations
      , refreshCanonicalFacts = canonical
      }

validateFacts :: [ImportedFact] -> Either Text ()
validateFacts [] = Left "statement import must contain at least one fact"
validateFacts facts
  | length facts > maxImportedFacts = Left "statement import exceeds the 10,000 fact limit"
  | otherwise = go Set.empty facts
  where
    go _ [] = Right ()
    go seen (fact : rest) = do
      key <- validateFact fact
      if Set.member key seen
        then Left "statement import contains duplicate fact identities"
        else go (Set.insert key seen) rest

validateFact ::
  ImportedFact -> Either Text (Text, Day, Day, PeriodType, Currency, StatementBasis, Text)
validateFact (ImportedFact rawConcept start end period currency amount basis rawScope) = do
  concept <- normalizeConcept rawConcept
  scope <- requireText "scope" rawScope
  if end < start
    then Left "fact period_end must be on or after period_start"
    else
      if concept == "capital_expenditure" && amount < 0
        then Left "capital_expenditure must be a non-negative cash outflow amount"
        else Right (concept, start, end, period, currency, basis, scope)

toCanonical :: CompanyId -> Provenance -> ImportedFact -> CanonicalFact
toCanonical company provenance (ImportedFact concept start end period currency amount basis scope) =
  CanonicalFact
    company
    (Text.toLower (Text.strip concept))
    start
    end
    period
    currency
    amount
    basis
    (Text.strip scope)
    "local-import-v1"
    provenance

toRaw :: ImportedFact -> RawObservation
toRaw (ImportedFact concept start end _ currency amount _ _) =
  RawObservation
    (Text.toLower (Text.strip concept))
    (Text.pack (show amount))
    (currencyCode currency <> " minor units")
    "local-import-v1"
    start
    end

validateSymbol :: Text -> Either Text Text
validateSymbol rawSymbol
  | Text.null symbol = Left "symbol cannot be blank"
  | Text.length symbol > 32 = Left "symbol exceeds 32 characters"
  | not (Text.all validSymbolCharacter symbol) = Left "symbol contains unsupported characters"
  | otherwise = Right symbol
  where
    symbol = Text.toUpper (Text.strip rawSymbol)

validSymbolCharacter :: Char -> Bool
validSymbolCharacter character =
  (character >= 'A' && character <= 'Z')
    || (character >= '0' && character <= '9')
    || character `elem` (".&_-" :: String)

normalizeConcept :: Text -> Either Text Text
normalizeConcept rawConcept
  | Text.null concept = Left "concept cannot be blank"
  | not (Text.all validConceptCharacter concept) =
      Left "concept must use letters, digits, and underscores"
  | otherwise = Right concept
  where
    concept = Text.toLower (Text.strip rawConcept)

validConceptCharacter :: Char -> Bool
validConceptCharacter character =
  (character >= 'a' && character <= 'z')
    || (character >= '0' && character <= '9')
    || character == '_'

requireText :: Text -> Text -> Either Text Text
requireText label value
  | Text.null (Text.strip value) = Left (label <> " cannot be blank")
  | otherwise = Right (Text.strip value)

validateSourceUri :: Text -> Either Text Text
validateSourceUri rawUri = do
  uri <- requireText "source_uri" rawUri
  if "https://" `Text.isPrefixOf` Text.toLower uri
    then Right uri
    else Left "source_uri must be an HTTPS link to the cited source"

parseTimestamp :: Text -> Text -> AesonTypes.Parser UTCTime
parseTimestamp label value =
  maybe
    (fail (Text.unpack (label <> " must be an ISO-8601 UTC timestamp ending in Z")))
    pure
    (parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (Text.unpack value))

parseDate :: Text -> Text -> AesonTypes.Parser Day
parseDate label value =
  maybe
    (fail (Text.unpack (label <> " must use YYYY-MM-DD")))
    pure
    (parseTimeM True defaultTimeLocale "%F" (Text.unpack value))

parsePeriodType :: Text -> AesonTypes.Parser PeriodType
parsePeriodType value = case Text.toLower value of
  "annual" -> pure Annual
  "quarterly" -> pure Quarterly
  "ttm" -> pure TrailingTwelveMonths
  _ -> fail "period_type must be annual, quarterly, or ttm"

parseCurrency :: Text -> AesonTypes.Parser Currency
parseCurrency value = case Text.toUpper value of
  "INR" -> pure INR
  "USD" -> pure USD
  "EUR" -> pure EUR
  _ -> fail "currency must be INR, USD, or EUR"

parseBasis :: Text -> AesonTypes.Parser StatementBasis
parseBasis value = case Text.toLower value of
  "standalone" -> pure Standalone
  "consolidated" -> pure Consolidated
  _ -> fail "basis must be standalone or consolidated"

currencyCode :: Currency -> Text
currencyCode currency = case currency of
  INR -> "INR"
  USD -> "USD"
  EUR -> "EUR"

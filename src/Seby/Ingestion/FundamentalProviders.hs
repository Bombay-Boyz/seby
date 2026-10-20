{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Fundamental statement adapters for providers whose APIs can be called
directly. They retain raw payloads and report source failures explicitly.
-}
module Seby.Ingestion.FundamentalProviders (
  FmpError (..),
  decodeFmpStatements,
  decodeAlphaVantageStatements,
  fetchFmpStatements,
  fetchAlphaVantageStatements,
) where

import Control.Exception (try)
import Data.Aeson (Value (..), eitherDecodeStrict', encode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Foldable (toList)
import Data.List (sortOn)
import Data.Maybe (catMaybes)
import Data.Scientific (Scientific, base10Exponent, coefficient)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Time (
  Day,
  UTCTime,
  addDays,
  addGregorianMonthsClip,
  addGregorianYearsClip,
  defaultTimeLocale,
  getCurrentTime,
  parseTimeM,
 )
import Network.HTTP.Client (
  HttpException,
  Manager,
  Request,
  httpLbs,
  managerResponseTimeout,
  newManager,
  parseRequest,
  responseBody,
  responseStatus,
  responseTimeoutMicro,
  setQueryString,
 )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Seby.Domain.Money (Currency (..))
import Seby.Domain.Types (
  CompanyId (..),
  PeriodType (..),
  Provenance (..),
  SourceId (..),
  StatementBasis (..),
  Symbol (..),
 )
import Seby.Ingestion.LocalStatements (maxImportedFacts)
import Seby.Persistence.SQLite (payloadSha256)
import Seby.Persistence.Types (
  CanonicalFact (..),
  RawObservation (..),
  RawPayload (..),
  RefreshBatch (..),
 )
import System.Timeout (timeout)

data FmpError
  = FmpMissingApiKey
  | FmpInvalidProviderSymbol
  | FmpNetworkFailure
  | FmpHttpFailure !Int
  | FmpProviderMessage !Text
  | FmpInvalidPayload !Text
  | FmpUnsupportedCurrency !Text
  | FmpNoSupportedFacts !Text
  | FmpPayloadTooLarge !Int
  | FmpInvalidAlphaVantagePayload !Text
  deriving stock (Eq, Show)

data StatementRow = StatementRow
  { rowDate :: !Day
  , rowPeriod :: !PeriodType
  , rowCurrency :: !Currency
  , rowFields :: !(KeyMap.KeyMap Value)
  }

data RawFact = RawFact !Text !Day !Day !PeriodType !Currency !Integer

maxProviderResponseBytes :: Int
maxProviderResponseBytes = 20 * 1024 * 1024

-- | Decode the two full provider responses without making a network request.
decodeFmpStatements ::
  ByteString -> ByteString -> UTCTime -> Symbol -> Symbol -> Either FmpError RefreshBatch
decodeFmpStatements incomePayload cashPayload retrievedAt companyTicker providerSymbol = do
  buildProviderBatch
    (SourceId "FINANCIAL_MODELING_PREP")
    "https://site.financialmodelingprep.com/developer/docs"
    "fmp-standard-statements-assumed-consolidated-v1"
    incomePayload
    cashPayload
    incomePayload
    cashPayload
    ("https://financialmodelingprep.com/stable/income-statement?symbol=" <> unSymbol providerSymbol)
    ("https://financialmodelingprep.com/stable/cash-flow-statement?symbol=" <> unSymbol providerSymbol)
    retrievedAt
    companyTicker
    providerSymbol

decodeAlphaVantageStatements ::
  ByteString -> ByteString -> UTCTime -> Symbol -> Symbol -> Either FmpError RefreshBatch
decodeAlphaVantageStatements incomePayload cashPayload retrievedAt companyTicker providerSymbol = do
  normalizedIncome <- normalizeAlphaVantagePayload incomePayload
  normalizedCash <- normalizeAlphaVantagePayload cashPayload
  buildProviderBatch
    (SourceId "ALPHA_VANTAGE")
    "https://www.alphavantage.co/documentation/"
    "alpha-vantage-statements-assumed-consolidated-v1"
    incomePayload
    cashPayload
    normalizedIncome
    normalizedCash
    ("https://www.alphavantage.co/query?function=INCOME_STATEMENT&symbol=" <> unSymbol providerSymbol)
    ("https://www.alphavantage.co/query?function=CASH_FLOW&symbol=" <> unSymbol providerSymbol)
    retrievedAt
    companyTicker
    providerSymbol

buildProviderBatch ::
  SourceId ->
  Text ->
  Text ->
  ByteString ->
  ByteString ->
  ByteString ->
  ByteString ->
  Text ->
  Text ->
  UTCTime ->
  Symbol ->
  Symbol ->
  Either FmpError RefreshBatch
buildProviderBatch source sourceInfo policyVersion incomeRaw cashRaw incomeNormalized cashNormalized incomeUri cashUri retrievedAt companyTicker providerSymbol = do
  incomeRows <- decodeStatementRows "income statement" incomeNormalized
  cashRows <- decodeStatementRows "cash-flow statement" cashNormalized
  let facts = concatMap incomeFacts incomeRows <> concatMap cashFacts cashRows
  if null facts
    then
      Left
        ( FmpNoSupportedFacts
            ("Provider returned no supported statement fields for " <> unSymbol providerSymbol)
        )
    else
      if length facts > maxImportedFacts
        then Left (FmpInvalidPayload "Provider response exceeds the 10,000 fact safety limit")
        else
          let payload = incomeRaw <> "\n--" <> TextEncoding.encodeUtf8 (unSourceId source) <> "-CASHFLOW--\n" <> cashRaw
              company = CompanyId (unSymbol companyTicker)
              digest = payloadSha256 payload
              rawProvenance = Provenance source (Just sourceInfo) retrievedAt Nothing digest
              incomeProvenance = Provenance source (Just incomeUri) retrievedAt Nothing (payloadSha256 incomeRaw)
              cashProvenance = Provenance source (Just cashUri) retrievedAt Nothing (payloadSha256 cashRaw)
              canonical =
                fmap (toCanonical company policyVersion incomeProvenance) (concatMap incomeFacts incomeRows)
                  <> fmap (toCanonical company policyVersion cashProvenance) (concatMap cashFacts cashRows)
              observations = fmap (toObservation policyVersion) facts
              runId = Text.toLower (unSourceId source) <> ":" <> digest <> ":" <> Text.pack (show retrievedAt)
           in Right
                RefreshBatch
                  { refreshRunId = runId
                  , refreshCompany = company
                  , refreshCompanyName = unSymbol companyTicker
                  , refreshAttemptedAt = retrievedAt
                  , refreshCompletedAt = retrievedAt
                  , refreshPayload = RawPayload rawProvenance payload
                  , refreshRawObservations = observations
                  , refreshCanonicalFacts = canonical
                  }

{- | Decode annual and quarterly reports returned by Alpha Vantage's documented
fundamental endpoints into the common normalized statement envelope. Raw
payloads remain untouched in the persisted batch.
-}
normalizeAlphaVantagePayload :: ByteString -> Either FmpError ByteString
normalizeAlphaVantagePayload payload = do
  value <- case eitherDecodeStrict' payload of
    Left message -> Left (FmpInvalidAlphaVantagePayload (Text.pack message))
    Right parsed -> Right parsed
  case value of
    Object fields -> case firstText ["Error Message", "Note", "Information"] fields of
      Just message -> Left (FmpInvalidAlphaVantagePayload message)
      Nothing -> do
        annual <- normalizeReports "annualReports" "FY" fields
        quarterly <- normalizeReports "quarterlyReports" "QUARTER" fields
        if null annual && null quarterly
          then Left (FmpInvalidAlphaVantagePayload "response contains no annual or quarterly report rows")
          else Right (LazyByteString.toStrict (encode (annual <> quarterly)))
    _ -> Left (FmpInvalidAlphaVantagePayload "response must be a JSON object")

normalizeReports :: Text -> Text -> KeyMap.KeyMap Value -> Either FmpError [Value]
normalizeReports fieldName periodLabel fields = case KeyMap.lookup (Key.fromText fieldName) fields of
  Just (Array rows) -> traverse normalizeRow (toList rows)
  _ -> Left (FmpInvalidAlphaVantagePayload ("missing expected " <> fieldName <> " array"))
  where
    normalizeRow (Object rowFields) = do
      dateValue <- case KeyMap.lookup "fiscalDateEnding" rowFields of
        Just (String value) | parseDate value /= Nothing -> Right value
        _ -> Left (FmpInvalidAlphaVantagePayload (fieldName <> " row has no valid fiscalDateEnding"))
      if KeyMap.lookup "reportedCurrency" rowFields == Nothing
        then Left (FmpInvalidAlphaVantagePayload (fieldName <> " row has no reportedCurrency"))
        else
          Right . Object $
            KeyMap.insert
              "period"
              (String periodLabel)
              (KeyMap.insert "date" (String dateValue) rowFields)
    normalizeRow _ = Left (FmpInvalidAlphaVantagePayload (fieldName <> " rows must be objects"))

-- | Retrieve income and cash-flow statements using the caller's FMP plan key.
fetchFmpStatements :: Text -> Symbol -> Symbol -> IO (Either FmpError RefreshBatch)
fetchFmpStatements apiKey companyTicker providerSymbol
  | Text.null (Text.strip apiKey) = pure (Left FmpMissingApiKey)
  | not (validProviderSymbol (unSymbol providerSymbol)) = pure (Left FmpInvalidProviderSymbol)
  | otherwise = do
      manager <-
        newManager tlsManagerSettings{managerResponseTimeout = responseTimeoutMicro (20 * 1000000)}
      income <- requestStatement manager apiKey "income-statement" providerSymbol
      case income of
        Left err -> pure (Left err)
        Right incomePayload -> do
          cash <- requestStatement manager apiKey "cash-flow-statement" providerSymbol
          case cash of
            Left err -> pure (Left err)
            Right cashPayload -> do
              retrievedAt <- getCurrentTime
              pure (decodeFmpStatements incomePayload cashPayload retrievedAt companyTicker providerSymbol)

{- | Retrieve the two documented Alpha Vantage fundamental statements. The
free account quota is shared with the rest of the key (currently 25 calls/day).
-}
fetchAlphaVantageStatements :: Text -> Symbol -> Symbol -> IO (Either FmpError RefreshBatch)
fetchAlphaVantageStatements apiKey companyTicker providerSymbol
  | Text.null (Text.strip apiKey) = pure (Left FmpMissingApiKey)
  | not (validProviderSymbol (unSymbol providerSymbol)) = pure (Left FmpInvalidProviderSymbol)
  | otherwise = do
      manager <-
        newManager tlsManagerSettings{managerResponseTimeout = responseTimeoutMicro (20 * 1000000)}
      income <- requestAlphaVantage manager apiKey "INCOME_STATEMENT" providerSymbol
      case income of
        Left err -> pure (Left err)
        Right incomePayload -> do
          cash <- requestAlphaVantage manager apiKey "CASH_FLOW" providerSymbol
          case cash of
            Left err -> pure (Left err)
            Right cashPayload -> do
              retrievedAt <- getCurrentTime
              pure
                (decodeAlphaVantageStatements incomePayload cashPayload retrievedAt companyTicker providerSymbol)

requestAlphaVantage :: Manager -> Text -> Text -> Symbol -> IO (Either FmpError ByteString)
requestAlphaVantage manager apiKey functionName providerSymbol = do
  prepared <- try (makeAlphaVantageRequest apiKey functionName providerSymbol)
  case prepared of
    Left (_ :: HttpException) -> pure (Left FmpNetworkFailure)
    Right request -> do
      fetched <- try (timeout (25 * 1000000) (httpLbs request manager))
      case fetched of
        Left (_ :: HttpException) -> pure (Left FmpNetworkFailure)
        Right Nothing -> pure (Left FmpNetworkFailure)
        Right (Just response)
          | statusCode (responseStatus response) == 429 -> pure (Left (FmpHttpFailure 429))
          | statusCode (responseStatus response) < 200 || statusCode (responseStatus response) >= 300 ->
              pure (Left (FmpHttpFailure (statusCode (responseStatus response))))
          | otherwise ->
              let payload = LazyByteString.toStrict (responseBody response)
               in if ByteString.length payload > maxProviderResponseBytes
                    then pure (Left (FmpPayloadTooLarge maxProviderResponseBytes))
                    else case providerError payload of
                      Just message -> pure (Left (FmpProviderMessage message))
                      Nothing -> pure (Right payload)

makeAlphaVantageRequest :: Text -> Text -> Symbol -> IO Request
makeAlphaVantageRequest apiKey functionName providerSymbol = do
  initial <- parseRequest "https://www.alphavantage.co/query"
  pure $
    setQueryString
      [ ("function", Just (TextEncoding.encodeUtf8 functionName))
      , ("symbol", Just (TextEncoding.encodeUtf8 (unSymbol providerSymbol)))
      , ("apikey", Just (TextEncoding.encodeUtf8 (Text.strip apiKey)))
      ]
      initial

requestStatement :: Manager -> Text -> Text -> Symbol -> IO (Either FmpError ByteString)
requestStatement manager apiKey endpoint ticker = do
  prepared <- try (makeRequest apiKey endpoint ticker)
  case prepared of
    Left (_ :: HttpException) -> pure (Left FmpNetworkFailure)
    Right request -> do
      fetched <- try (timeout (25 * 1000000) (httpLbs request manager))
      case fetched of
        Left (_ :: HttpException) -> pure (Left FmpNetworkFailure)
        Right Nothing -> pure (Left FmpNetworkFailure)
        Right (Just response)
          | statusCode (responseStatus response) == 429 -> pure (Left (FmpHttpFailure 429))
          | statusCode (responseStatus response) < 200 || statusCode (responseStatus response) >= 300 ->
              pure (Left (FmpHttpFailure (statusCode (responseStatus response))))
          | otherwise ->
              let payload = LazyByteString.toStrict (responseBody response)
               in if ByteString.length payload > maxProviderResponseBytes
                    then pure (Left (FmpPayloadTooLarge maxProviderResponseBytes))
                    else case providerError payload of
                      Just message -> pure (Left (FmpProviderMessage message))
                      Nothing -> pure (Right payload)

makeRequest :: Text -> Text -> Symbol -> IO Request
makeRequest apiKey endpoint ticker = do
  let base = "https://financialmodelingprep.com/stable/" <> Text.unpack endpoint
  initial <- parseRequest base
  pure $
    setQueryString
      [ ("symbol", Just (TextEncoding.encodeUtf8 (unSymbol ticker)))
      , ("limit", Just "40")
      , ("apikey", Just (TextEncoding.encodeUtf8 (Text.strip apiKey)))
      ]
      initial

providerError :: ByteString -> Maybe Text
providerError payload = case eitherDecodeStrict' payload of
  Right (Object object) ->
    firstText ["Error Message", "Error", "message", "error", "Note", "Information"] object
  _ -> Nothing

decodeStatementRows :: Text -> ByteString -> Either FmpError [StatementRow]
decodeStatementRows label payload = do
  value <- case eitherDecodeStrict' payload of
    Left message -> Left (FmpInvalidPayload (label <> " JSON is invalid: " <> Text.pack message))
    Right parsed -> Right parsed
  array <- case value of
    Array values -> Right (toList values)
    _ -> Left (FmpInvalidPayload (label <> " response must be a JSON array"))
  rows <- traverse parseRow array
  pure (sortOn rowDate rows)
  where
    parseRow (Object fields) = do
      dateText <- requireText label ["date", "fiscalDateEnding"] fields
      date <-
        maybe (Left (FmpInvalidPayload (label <> " contains an invalid date"))) Right (parseDate dateText)
      periodText <- requireText label ["period"] fields
      period <- parsePeriod periodText
      reportedCurrency <- requireText label ["reportedCurrency", "currency"] fields
      currency <- parseCurrency reportedCurrency
      pure (StatementRow date period currency fields)
    parseRow _ = Left (FmpInvalidPayload (label <> " rows must be JSON objects"))

incomeFacts :: StatementRow -> [RawFact]
incomeFacts row =
  factsFor
    row
    [ ("revenue", ["revenue", "totalRevenue"])
    , ("gross_profit", ["grossProfit"])
    , ("operating_profit", ["operatingIncome", "operatingIncomeLoss"])
    , ("net_profit", ["netIncome", "netIncomeFromContinuingOperations"])
    ]

cashFacts :: StatementRow -> [RawFact]
cashFacts row =
  factsFor
    row
    [
      ( "operating_cash_flow"
      , ["operatingCashFlow", "operatingCashflow", "netCashProvidedByOperatingActivities"]
      )
    ,
      ( "capital_expenditure"
      , ["capitalExpenditure", "capitalExpenditures", "investmentsInPropertyPlantAndEquipment"]
      )
    ]

factsFor :: StatementRow -> [(Text, [Text])] -> [RawFact]
factsFor row concepts = catMaybes (fmap factFor concepts)
  where
    factFor (concept, aliases) = do
      amount <- firstMoney aliases (rowFields row)
      let RawPeriod start end = periodBounds (rowDate row) (rowPeriod row)
          normalizedAmount = if concept == "capital_expenditure" then abs amount else amount
      pure (RawFact concept start end (rowPeriod row) (rowCurrency row) normalizedAmount)

data RawPeriod = RawPeriod !Day !Day

periodBounds :: Day -> PeriodType -> RawPeriod
periodBounds end period = case period of
  Annual -> RawPeriod (addDays 1 (addGregorianYearsClip (-1) end)) end
  Quarterly -> RawPeriod (addDays 1 (addGregorianMonthsClip (-3) end)) end
  TrailingTwelveMonths -> RawPeriod (addDays 1 (addGregorianYearsClip (-1) end)) end

toCanonical :: CompanyId -> Text -> Provenance -> RawFact -> CanonicalFact
toCanonical company policyVersion provenance (RawFact concept start end period currency amount) =
  CanonicalFact
    company
    concept
    start
    end
    period
    currency
    amount
    Consolidated
    "group"
    policyVersion
    provenance

toObservation :: Text -> RawFact -> RawObservation
toObservation policyVersion (RawFact concept start end _period currency amount) =
  RawObservation
    concept
    (Text.pack (show amount))
    (currencyText currency <> " minor units")
    policyVersion
    start
    end

firstMoney :: [Text] -> KeyMap.KeyMap Value -> Maybe Integer
firstMoney [] _ = Nothing
firstMoney (key : rest) fields = case KeyMap.lookup (Key.fromText key) fields of
  Just (Number value) -> Just (round (scientificRational value * 100))
  Just (String value) -> case reads (Text.unpack (Text.filter (/= ',') value)) :: [(Scientific, String)] of
    [(number, "")] -> Just (round (scientificRational number * 100))
    _ -> firstMoney rest fields
  _ -> firstMoney rest fields

scientificRational :: Scientific -> Rational
scientificRational value = fromInteger (coefficient value) * (10 ^^ base10Exponent value)

requireText :: Text -> [Text] -> KeyMap.KeyMap Value -> Either FmpError Text
requireText label keys fields =
  maybe
    (Left (FmpInvalidPayload (label <> " row is missing required field " <> Text.intercalate "/" keys)))
    Right
    (optionalText keys fields)

optionalText :: [Text] -> KeyMap.KeyMap Value -> Maybe Text
optionalText [] _ = Nothing
optionalText (key : rest) fields = case KeyMap.lookup (Key.fromText key) fields of
  Just (String value) -> Just value
  Just (Number value) -> Just (Text.pack (show value))
  _ -> optionalText rest fields

firstText :: [Text] -> KeyMap.KeyMap Value -> Maybe Text
firstText = optionalText

parseDate :: Text -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%F" . Text.unpack

parsePeriod :: Text -> Either FmpError PeriodType
parsePeriod value = case Text.toUpper value of
  "FY" -> Right Annual
  "ANNUAL" -> Right Annual
  "Q1" -> Right Quarterly
  "Q2" -> Right Quarterly
  "Q3" -> Right Quarterly
  "Q4" -> Right Quarterly
  "QUARTER" -> Right Quarterly
  "TTM" -> Right TrailingTwelveMonths
  _ -> Left (FmpInvalidPayload ("unsupported FMP reporting period: " <> value))

parseCurrency :: Text -> Either FmpError Currency
parseCurrency value = case Text.toUpper value of
  "INR" -> Right INR
  "USD" -> Right USD
  "EUR" -> Right EUR
  _ -> Left (FmpUnsupportedCurrency value)

currencyText :: Currency -> Text
currencyText currency = case currency of
  INR -> "INR"
  USD -> "USD"
  EUR -> "EUR"

validProviderSymbol :: Text -> Bool
validProviderSymbol value =
  not (Text.null value)
    && Text.length value <= 32
    && Text.all validCharacter value
  where
    validCharacter character =
      (character >= 'A' && character <= 'Z')
        || (character >= '0' && character <= '9')
        || character `elem` ("._-&" :: String)

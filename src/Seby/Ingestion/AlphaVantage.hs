{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | Alpha Vantage daily price adapter.

This adapter uses only the documented free compact endpoint (latest 100 raw
daily observations). It does not fetch adjusted history or use an unofficial
endpoint. Network and provider failures remain explicit to the caller.
-}
module Seby.Ingestion.AlphaVantage (
  AlphaVantageConfig,
  mkAlphaVantageConfig,
  AlphaVantageError (..),
  FetchedDailyPrices (..),
  PriceRefreshError (..),
  PriceRefreshSummary (..),
  dailyPriceRefreshBatch,
  fetchDailyPrices,
  decodeDailyPrices,
  refreshDailyPrices,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (FromJSON (..), Value (..), eitherDecodeStrict', withObject, (.:))
import Data.Aeson.Key (toText)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as AesonTypes
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List (sortOn)
import Data.Ratio (Ratio, (%))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Time (Day, UTCTime, defaultTimeLocale, getCurrentTime, parseTimeM)
import Network.HTTP.Client (
  HttpException,
  Manager,
  Request,
  Response,
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
import Seby.Domain.Money (Currency (INR))
import Seby.Domain.Types (CompanyId, Provenance (..), SecurityId, SourceId (..))
import Seby.Market.Prices (DailyPrice, PriceError, mkDailyPrice)
import Seby.Persistence.Repository (FactRepository (..))
import Seby.Persistence.Types (RawPayload (..), RefreshBatch (..), RefreshFailure (..), StoreError)
import System.Timeout (timeout)

data AlphaVantageConfig = AlphaVantageConfig !Text

-- | API key remains encapsulated and must not be included in logs or provenance.
mkAlphaVantageConfig :: Text -> Either AlphaVantageError AlphaVantageConfig
mkAlphaVantageConfig key
  | Text.null (Text.strip key) = Left MissingApiKey
  | otherwise = Right (AlphaVantageConfig (Text.strip key))

data AlphaVantageError
  = MissingApiKey
  | InvalidProviderSymbol
  | NetworkFailure !Text
  | HttpFailure !Int
  | ProviderMessage !Text
  | InvalidProviderPayload !Text
  | InvalidQuote !Day !PriceError
  deriving stock (Eq, Show)

data FetchedDailyPrices = FetchedDailyPrices
  { fetchedSecurityId :: !SecurityId
  , fetchedProviderSymbol :: !Text
  , fetchedAttemptedAt :: !UTCTime
  , fetchedProvenance :: !Provenance
  , fetchedPayload :: !ByteString
  , fetchedPrices :: ![DailyPrice]
  }
  deriving stock (Eq, Show)

data ProviderQuote
  = ProviderQuote !Day !(Ratio Integer) !(Ratio Integer) !(Ratio Integer) !(Ratio Integer) !Integer

instance FromJSON ProviderQuote where
  parseJSON = withObject "daily quote" $ \fields -> do
    case KeyMap.lookup "date" fields of
      Just (String dateText) -> do
        day <- maybe (fail "invalid quote date") pure (parseDay dateText)
        openText <- fields .: "1. open"
        highText <- fields .: "2. high"
        lowText <- fields .: "3. low"
        closeText <- fields .: "4. close"
        open <- parseDecimalField "1. open" openText
        high <- parseDecimalField "2. high" highText
        low <- parseDecimalField "3. low" lowText
        close <- parseDecimalField "4. close" closeText
        volumeText <- fields .: "5. volume"
        volume <- maybe (fail "invalid integer volume") pure (readInteger volumeText)
        pure (ProviderQuote day open high low close volume)
      _ -> fail "missing quote date"

data Envelope = Envelope ![ProviderQuote]

instance FromJSON Envelope where
  parseJSON = withObject "Alpha Vantage response" $ \fields -> do
    series <- fields .: "Time Series (Daily)"
    quotes <- traverse parseQuote (KeyMap.toList series)
    pure (Envelope (sortOn quoteDay quotes))
    where
      parseQuote (key, value) =
        case value of
          Object quote -> do
            let withDate = KeyMap.insert "date" (String (toText key)) quote
            parseJSON (Object withDate)
          _ -> fail "daily series entry must be an object"

      quoteDay (ProviderQuote day _ _ _ _ _) = day

firstProviderMessage :: KeyMap.KeyMap Value -> Maybe Text
firstProviderMessage fields = firstPresent ["Error Message", "Note", "Information"]
  where
    firstPresent [] = Nothing
    firstPresent (key : rest) = case KeyMap.lookup key fields of
      Just (String message) -> Just message
      _ -> firstPresent rest

-- | Fetch one symbol's latest compact daily history with bounded network retries.
fetchDailyPrices ::
  AlphaVantageConfig ->
  SecurityId ->
  Text ->
  IO (Either AlphaVantageError FetchedDailyPrices)
fetchDailyPrices (AlphaVantageConfig apiKey) securityId providerSymbol = do
  if not (validProviderSymbol providerSymbol)
    then pure (Left InvalidProviderSymbol)
    else do
      manager <-
        newManager tlsManagerSettings{managerResponseTimeout = responseTimeoutMicro (15 * 1000000)}
      requestedAt <- getCurrentTime
      requestResult <- try @HttpException (makeRequest apiKey providerSymbol)
      case requestResult of
        Left _ -> pure (Left (NetworkFailure "Alpha Vantage request could not be prepared"))
        Right request -> do
          response <- requestWithRetry manager request 3
          case response of
            Left err -> pure (Left err)
            Right result -> do
              retrievedAt <- getCurrentTime
              finishFetch requestedAt retrievedAt securityId providerSymbol result

makeRequest :: Text -> Text -> IO Request
makeRequest apiKey providerSymbol = do
  baseRequest <- parseRequest "https://www.alphavantage.co/query"
  pure $
    setQueryString
      [ ("function", Just "TIME_SERIES_DAILY")
      , ("symbol", Just (TextEncoding.encodeUtf8 providerSymbol))
      , ("outputsize", Just "compact")
      , ("apikey", Just (TextEncoding.encodeUtf8 apiKey))
      ]
      baseRequest

requestWithRetry ::
  Manager -> Request -> Int -> IO (Either AlphaVantageError (Response LazyByteString.ByteString))
requestWithRetry manager request attemptsLeft = do
  result <- try @HttpException (timeout (20 * 1000000) (httpLbs request manager))
  case result of
    Left _
      | attemptsLeft > 1 -> retryAfter attemptsLeft
      | otherwise -> pure (Left (NetworkFailure "Alpha Vantage network request failed"))
    Right Nothing
      | attemptsLeft > 1 -> retryAfter attemptsLeft
      | otherwise -> pure (Left (NetworkFailure "Alpha Vantage request timed out"))
    Right (Just response)
      | statusCode (responseStatus response) >= 500 && attemptsLeft > 1 -> retryAfter attemptsLeft
      | statusCode (responseStatus response) >= 500 ->
          pure (Left (HttpFailure (statusCode (responseStatus response))))
      | statusCode (responseStatus response) < 200 || statusCode (responseStatus response) >= 300 ->
          pure (Left (HttpFailure (statusCode (responseStatus response))))
      | otherwise -> pure (Right response)
  where
    retryAfter remaining = do
      threadDelay (retryDelayMicros (4 - remaining))
      requestWithRetry manager request (remaining - 1)

retryDelayMicros :: Int -> Int
retryDelayMicros attempt = min 2000000 (250000 * (2 ^ attempt))

finishFetch ::
  UTCTime ->
  UTCTime ->
  SecurityId ->
  Text ->
  Response LazyByteString.ByteString ->
  IO (Either AlphaVantageError FetchedDailyPrices)
finishFetch attemptedAt retrievedAt securityId providerSymbol response = do
  let payload = LazyByteString.toStrict (responseBody response)
  case decodeDailyPrices payload of
    Left err -> pure (Left err)
    Right prices -> do
      let digest = sha256 payload
          safeUri =
            "https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol="
              <> providerSymbol
              <> "&outputsize=compact"
          provenance = Provenance (SourceId "ALPHA_VANTAGE") (Just safeUri) retrievedAt Nothing digest
      pure . Right $ FetchedDailyPrices securityId providerSymbol attemptedAt provenance payload prices

-- | Decode and validate the free endpoint fixture shape without a network call.
decodeDailyPrices :: ByteString -> Either AlphaVantageError [DailyPrice]
decodeDailyPrices payload = do
  value <- case eitherDecodeStrict' payload of
    Left message -> Left (InvalidProviderPayload (Text.pack message))
    Right value -> Right value
  case value of
    Object fields -> case firstProviderMessage fields of
      Just message -> Left (ProviderMessage message)
      Nothing -> decodeEnvelope value
    _ -> Left (InvalidProviderPayload "Alpha Vantage response must be a JSON object")

decodeEnvelope :: Value -> Either AlphaVantageError [DailyPrice]
decodeEnvelope value = do
  envelope <- case AesonTypes.parseEither parseJSON value of
    Left message -> Left (InvalidProviderPayload (Text.pack message))
    Right parsed -> Right parsed
  traverse fromProviderQuote (envelopeQuotes envelope)

envelopeQuotes :: Envelope -> [ProviderQuote]
envelopeQuotes (Envelope quotes) = quotes

fromProviderQuote :: ProviderQuote -> Either AlphaVantageError DailyPrice
fromProviderQuote (ProviderQuote day open high low close volume) =
  case mkDailyPrice INR day open high low close volume of
    Left err -> Left (InvalidQuote day err)
    Right price -> Right price

parseDay :: Text -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%F" . Text.unpack

readInteger :: Text -> Maybe Integer
readInteger value
  | Text.null value || not (Text.all isAsciiDigit value) = Nothing
  | otherwise = case reads (Text.unpack value) of
      [(number, "")] -> Just number
      _ -> Nothing

parseDecimalField :: Text -> Text -> AesonTypes.Parser (Ratio Integer)
parseDecimalField field value =
  maybe
    (fail (Text.unpack field <> " must be a non-negative decimal string"))
    pure
    (parseDecimal value)

parseDecimal :: Text -> Maybe (Ratio Integer)
parseDecimal value = case Text.splitOn "." value of
  [whole] -> (% 1) <$> readInteger whole
  [whole, fractional]
    | Text.null fractional -> Nothing
    | otherwise -> do
        wholeValue <- readInteger whole
        fractionalValue <- readInteger fractional
        let scale = 10 ^ Text.length fractional
        pure ((wholeValue * scale + fractionalValue) % scale)
  _ -> Nothing

isAsciiDigit :: Char -> Bool
isAsciiDigit character = character >= '0' && character <= '9'

sha256 :: ByteString -> Text
sha256 = Text.pack . show . (hash :: ByteString -> Digest SHA256)

{- | Store the exact response body and its digest through the existing atomic
refresh transaction; prices remain separately typed for later analysis.
-}
dailyPriceRefreshBatch :: Text -> CompanyId -> FetchedDailyPrices -> RefreshBatch
dailyPriceRefreshBatch runId company fetched =
  RefreshBatch
    { refreshRunId = runId
    , refreshCompany = company
    , refreshAttemptedAt = fetchedAttemptedAt fetched
    , refreshCompletedAt = provenanceRetrievedAt provenance
    , refreshPayload = RawPayload provenance (fetchedPayload fetched)
    , refreshRawObservations = []
    , refreshCanonicalFacts = []
    }
  where
    provenance = fetchedProvenance fetched

validProviderSymbol :: Text -> Bool
validProviderSymbol symbol =
  not (Text.null symbol)
    && Text.length symbol <= 32
    && Text.all isProviderSymbolCharacter symbol

isProviderSymbolCharacter :: Char -> Bool
isProviderSymbolCharacter character =
  (character >= 'A' && character <= 'Z')
    || (character >= 'a' && character <= 'z')
    || (character >= '0' && character <= '9')
    || character == '.'
    || character == '-'
    || character == '_'

data PriceRefreshError
  = PriceFetchFailed !AlphaVantageError
  | PriceStoreFailed !StoreError
  | PriceFailureRecordFailed !StoreError
  deriving stock (Eq, Show)

data PriceRefreshSummary = PriceRefreshSummary
  { priceRefreshCompanyId :: !CompanyId
  , priceRefreshSecurityId :: !SecurityId
  , priceRefreshObservationCount :: !Int
  , priceRefreshTimestamp :: !UTCTime
  }
  deriving stock (Eq, Show)

{- | Fetch and atomically retain the original provider payload; failed fetches
are recorded against the existing source-health state.
-}
refreshDailyPrices ::
  FactRepository ->
  AlphaVantageConfig ->
  Text ->
  CompanyId ->
  SecurityId ->
  Text ->
  IO (Either PriceRefreshError PriceRefreshSummary)
refreshDailyPrices repository config runId company security providerSymbol = do
  fetched <- fetchDailyPrices config security providerSymbol
  case fetched of
    Left fetchError -> do
      failedAt <- getCurrentTime
      let failure =
            RefreshFailure
              runId
              company
              (SourceId "ALPHA_VANTAGE")
              failedAt
              (Text.pack (show fetchError))
      recorded <- recordRefreshFailure repository failure
      pure $ case recorded of
        Left storeError -> Left (PriceFailureRecordFailed storeError)
        Right () -> Left (PriceFetchFailed fetchError)
    Right history -> do
      let batch = dailyPriceRefreshBatch runId company history
      stored <- storeRefreshBatch repository batch
      pure $ case stored of
        Left storeError -> Left (PriceStoreFailed storeError)
        Right () ->
          Right
            PriceRefreshSummary
              { priceRefreshCompanyId = company
              , priceRefreshSecurityId = security
              , priceRefreshObservationCount = length (fetchedPrices history)
              , priceRefreshTimestamp = provenanceRetrievedAt (fetchedProvenance history)
              }

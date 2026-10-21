{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Offline decoder for user-supplied, sourced portfolio snapshots.
module Seby.Ingestion.Portfolio (
  decodePortfolioSnapshot,
) where

import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (FromJSON (..), eitherDecodeStrict', withObject, (.:), (.:?))
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Char (isAsciiUpper, isDigit)
import Data.Ratio (denominator, numerator)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, UTCTime)
import Seby.Analysis.Portfolio (PortfolioPosition, mkPortfolioPosition)
import Seby.Domain.Money (Currency (..), Money, mkMoney)
import Seby.Domain.Types (Provenance (..), SourceId (..), Symbol (..))

data SnapshotDocument = SnapshotDocument !Text !(Maybe Text) !UTCTime ![SnapshotPosition]
  deriving stock (Eq, Show)

data SnapshotPosition = SnapshotPosition !Text !Scientific !Scientific !Scientific !Currency !Day
  deriving stock (Eq, Show)

instance FromJSON SnapshotDocument where
  parseJSON = withObject "PortfolioSnapshot" $ \object -> do
    source <- object .: "source"
    sourceUri <- object .:? "source_url"
    retrievedAt <- object .: "retrieved_at"
    positions <- object .: "positions"
    if Text.null (Text.strip source)
      then fail "source must not be blank"
      else case sourceUri of
        Just uri
          | not ("https://" `Text.isPrefixOf` Text.toLower uri) ->
              fail "source_url must use HTTPS"
        _ -> pure (SnapshotDocument source sourceUri retrievedAt positions)

instance FromJSON SnapshotPosition where
  parseJSON = withObject "PortfolioPosition" $ \object -> do
    symbol <- object .: "symbol"
    quantity <- object .: "quantity"
    averageCost <- object .: "average_cost"
    marketPrice <- object .: "market_price"
    currencyText <- object .: "currency"
    priceDate <- object .: "price_date"
    currency <- parseCurrency currencyText
    pure (SnapshotPosition symbol quantity averageCost marketPrice currency priceDate)

parseCurrency :: Text -> Parser Currency
parseCurrency value = case Text.toUpper value of
  "INR" -> pure INR
  "USD" -> pure USD
  "EUR" -> pure EUR
  _ -> fail "currency must be INR, USD, or EUR"

decodePortfolioSnapshot :: ByteString -> Either Text [PortfolioPosition]
decodePortfolioSnapshot bytes
  | ByteString.length bytes > maximumSnapshotBytes =
      Left "portfolio snapshot exceeds the 8 MiB input limit"
  | otherwise = do
      document <-
        either (Left . Text.pack) Right (eitherDecodeStrict' bytes :: Either String SnapshotDocument)
      case document of
        SnapshotDocument source sourceUri retrievedAt positions ->
          traverse (makePosition source sourceUri retrievedAt digest) positions
  where
    digest = Text.pack (show (hash bytes :: Digest SHA256))

maximumSnapshotBytes :: Int
maximumSnapshotBytes = 8 * 1024 * 1024

makePosition ::
  Text -> Maybe Text -> UTCTime -> Text -> SnapshotPosition -> Either Text PortfolioPosition
makePosition source sourceUri retrievedAt digest (SnapshotPosition rawSymbol quantity averageCost marketPrice currency priceDate) = do
  symbol <- validateSymbol rawSymbol
  shares <- positiveQuantity symbol quantity
  cost <- exactMoney "average_cost" currency averageCost
  price <- exactMoney "market_price" currency marketPrice
  let provenance = Provenance (SourceId source) sourceUri retrievedAt Nothing digest
  case mkPortfolioPosition symbol shares cost price priceDate provenance of
    Left reason -> Left (Text.pack (show reason))
    Right position -> Right position

validateSymbol :: Text -> Either Text Symbol
validateSymbol raw
  | Text.null normalized = Left "symbol must not be blank"
  | Text.any (not . isAllowed) normalized =
      Left "symbol contains unsupported characters"
  | otherwise = Right (Symbol normalized)
  where
    normalized = Text.toUpper (Text.strip raw)
    isAllowed character =
      isAsciiUpper character || isDigit character || character `elem` (".-&" :: String)

positiveQuantity :: Symbol -> Scientific -> Either Text Rational
positiveQuantity symbol value
  | toRational value <= 0 = Left ("quantity must be positive for " <> unSymbol symbol)
  | otherwise = Right (toRational value)

exactMoney :: Text -> Currency -> Scientific -> Either Text Money
exactMoney field currency value
  | paiseDenominator /= 1 = Left (field <> " must have no more than two decimal places")
  | numerator paise <= 0 = Left (field <> " must be positive")
  | otherwise = Right (mkMoney currency (numerator paise))
  where
    paise = toRational value * 100
    paiseDenominator = denominator paise

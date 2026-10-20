{-# LANGUAGE OverloadedStrings #-}

module Seby.AlphaVantageSpec (runTests) where

import Data.ByteString.Char8 qualified as ByteString
import Data.List (intercalate)
import Data.Time.Calendar (fromGregorian)
import Seby.Ingestion.AlphaVantage (AlphaVantageError (..), decodeDailyPrices)
import Seby.Market.Prices (dailyPriceClose, dailyPriceDate, dailyPriceOpen, dailyPriceVolume)

runTests :: IO ()
runTests = do
  let oneQuote = "{\"Time Series (Daily)\":{\"2026-10-19\":{\"1. open\":\"10.25\",\"2. high\":\"12.50\",\"3. low\":\"9.75\",\"4. close\":\"11.00\",\"5. volume\":\"123456\"}}}"
  case decodeDailyPrices (ByteString.pack oneQuote) of
    Right [quote] -> do
      assert "decimal parsed exactly" (dailyPriceOpen quote == 41 / 4)
      assert "close parsed exactly" (dailyPriceClose quote == 11)
      assert "volume retained" (dailyPriceVolume quote == 123456)
      assert "date retained" (dailyPriceDate quote == fromGregorian 2026 10 19)
    other -> failTest ("valid daily quote rejected: " <> show other)
  assert "provider throttle reported" $
    decodeDailyPrices (ByteString.pack "{\"Note\":\"call frequency exceeded\"}")
      == Left (ProviderMessage "call frequency exceeded")
  assert "bad JSON rejected" $
    isInvalid (decodeDailyPrices (ByteString.pack "{broken"))
  assert "zero close rejected as invalid quote" $
    isInvalid (decodeDailyPrices (ByteString.pack (quoteJson "0.00" "10.00" "8.00" "0.00" "1")))
  assert "high below low rejected" $
    isInvalid (decodeDailyPrices (ByteString.pack (quoteJson "9" "8" "10" "9" "1")))
  assert "empty series is a valid empty result" $
    decodeDailyPrices (ByteString.pack "{\"Time Series (Daily)\":{}}") == Right []
  let stress = decodeDailyPrices (ByteString.pack (seriesJson 100))
  case stress of
    Right (first : rest) -> do
      assert "100 row response parsed" (length rest + 1 == 100)
      case reverse rest of
        lastQuote : _ -> assert "response sorted chronologically" (dailyPriceDate first < dailyPriceDate lastQuote)
        [] -> failTest "stress response unexpectedly had one row"
    Right [] -> failTest "stress response unexpectedly empty"
    Left err -> failTest ("100 row response rejected: " <> show err)
  putStrLn "Alpha Vantage parser tests passed (including malformed, outlier, and 100-row cases)."

quoteJson :: String -> String -> String -> String -> String -> String
quoteJson open high low close volume =
  "{\"Time Series (Daily)\":{\"2026-10-19\":{\"1. open\":\"" <> open
    <> "\",\"2. high\":\"" <> high <> "\",\"3. low\":\"" <> low
    <> "\",\"4. close\":\"" <> close <> "\",\"5. volume\":\"" <> volume <> "\"}}}"

seriesJson :: Int -> String
seriesJson count = "{\"Time Series (Daily)\":{" <> intercalate "," rows <> "}}"
  where
    rows =
      [ "\"2026-" <> twoDigits month <> "-" <> twoDigits day
          <> "\":{\"1. open\":\"1\",\"2. high\":\"2\",\"3. low\":\"0.5\",\"4. close\":\"1.5\",\"5. volume\":\"0\"}"
      | index <- [0 .. count - 1]
      , let month = 1 + index `div` 28
      , let day = 1 + index `mod` 28
      ]
    twoDigits value = if value < 10 then '0' : show value else show value

isInvalid :: Either AlphaVantageError a -> Bool
isInvalid (Left (InvalidQuote _ _)) = True
isInvalid (Left (InvalidProviderPayload _)) = True
isInvalid _ = False

assert :: String -> Bool -> IO ()
assert label condition = if condition then pure () else failTest ("failed: " <> label)

failTest :: String -> IO a
failTest = fail

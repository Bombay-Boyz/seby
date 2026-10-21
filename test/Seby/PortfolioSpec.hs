{-# LANGUAGE OverloadedStrings #-}

module Seby.PortfolioSpec (runTests) where

import Data.ByteString.Char8 qualified as ByteString
import Data.Ratio ((%))
import Data.Text qualified as Text
import Seby.Analysis.Portfolio
import Seby.Domain.Money (Currency (..), toPaise)
import Seby.Domain.Types (Symbol (..))
import Seby.Ingestion.Portfolio (decodePortfolioSnapshot)
import Seby.Persistence.SQLite (payloadSha256)
import Seby.Reporting.PortfolioMarkdown (renderPortfolioMarkdown)

runTests :: IO ()
runTests = do
  positions <- decodeOrFail fixture
  summaries <- expectRight "valid portfolio aggregation" (summarizePortfolio positions)
  emptyPositions <- decodeOrFail emptyFixture
  assert "currency buckets are deterministic" (map summaryCurrency summaries == [INR, USD])
  case summaries of
    [inrSummary, usdSummary] -> do
      assert "INR basis sums exactly" (toPaise (summaryTotalCostBasis inrSummary) == 200000)
      assert "INR market value sums exactly" (toPaise (summaryTotalMarketValue inrSummary) == 230000)
      assert "INR unrealized gain sums exactly" (toPaise (summaryTotalUnrealizedGain inrSummary) == 30000)
      assert
        "positions sort by ticker"
        (map summarySymbol (summaryPositions inrSummary) == [Symbol "INFY", Symbol "TCS"])
      assert "return on cost is exact" (summaryReturnOnCost (last (summaryPositions inrSummary)) == 1 % 5)
      assert
        "weights are exact and sum to one per currency"
        (sum (map summaryPortfolioWeight (summaryPositions inrSummary)) == 1)
      assert
        "foreign currency positions stay in their own bucket"
        (summaryCurrency usdSummary == USD && toPaise (summaryTotalMarketValue usdSummary) == 7200)
      let markdown = renderPortfolioMarkdown summaries
      assert
        "report includes source citation and digest"
        ("broker-export" `Text.isInfixOf` markdown && payloadSha256 fixture `Text.isInfixOf` markdown)
      assert
        "report explains values and dated market marks"
        ("Unrealized gain" `Text.isInfixOf` markdown && "2026-10-20" `Text.isInfixOf` markdown)
      assert "report shows source retrieval time" ("2026-10-21 09:30:00 UTC" `Text.isInfixOf` markdown)
    _ -> fail "expected INR and USD summary buckets"
  assert "empty snapshots are rejected by the summary" (isLeft (summarizePortfolio emptyPositions))
  assert "invalid JSON is rejected" (isLeft (decodePortfolioSnapshot "{"))
  assert
    "non-HTTPS source URLs are rejected"
    ( isLeft
        ( decodePortfolioSnapshot
            (replace "https://broker.example/export" "http://broker.example/export" fixture)
        )
    )
  assert
    "prices with more than two decimal places are rejected"
    (isLeft (decodePortfolioSnapshot (replace "\"market_price\":120" "\"market_price\":120.001" fixture)))
  assert
    "duplicate symbols are rejected"
    (isDuplicate (summarizePortfolio (positions <> take 1 positions)))
  assert
    "non-positive quantities are rejected"
    (isLeft (decodePortfolioSnapshot (replace "\"quantity\":10" "\"quantity\":0" fixture)))
  tinyPosition <-
    decodeOrFail (replace "\"quantity\":10" "\"quantity\":0.000001" fixture)
  assert
    "positions that round to zero minor units are rejected"
    (isLeft (summarizePortfolio tinyPosition))
  putStrLn
    "Portfolio snapshot tests passed (exact arithmetic, currencies, provenance, rendering, and invalid inputs)."

decodeOrFail :: ByteString.ByteString -> IO [PortfolioPosition]
decodeOrFail = either (fail . Text.unpack) pure . decodePortfolioSnapshot

expectRight :: String -> Either errorValue value -> IO value
expectRight _ (Right value) = pure value
expectRight label (Left _) = fail (label <> " unexpectedly failed")

isLeft :: Either errorValue value -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

isDuplicate :: Either PortfolioError value -> Bool
isDuplicate (Left (DuplicatePortfolioSymbol _)) = True
isDuplicate _ = False

assert :: String -> Bool -> IO ()
assert label condition =
  if condition then pure () else fail ("Portfolio test failed: " <> label)

replace ::
  ByteString.ByteString -> ByteString.ByteString -> ByteString.ByteString -> ByteString.ByteString
replace old new =
  ByteString.pack
    . Text.unpack
    . Text.replace (Text.pack (ByteString.unpack old)) (Text.pack (ByteString.unpack new))
    . Text.pack
    . ByteString.unpack

fixture :: ByteString.ByteString
fixture =
  ByteString.pack
    "{\"source\":\"broker-export\",\"source_url\":\"https://broker.example/export\",\"retrieved_at\":\"2026-10-21T09:30:00Z\",\"positions\":[{\"symbol\":\"TCS\",\"quantity\":10,\"average_cost\":100,\"market_price\":120,\"currency\":\"INR\",\"price_date\":\"2026-10-20\"},{\"symbol\":\"INFY\",\"quantity\":5,\"average_cost\":200,\"market_price\":220,\"currency\":\"INR\",\"price_date\":\"2026-10-20\"},{\"symbol\":\"MSFT\",\"quantity\":4,\"average_cost\":20,\"market_price\":18,\"currency\":\"USD\",\"price_date\":\"2026-10-20\"}]}"

emptyFixture :: ByteString.ByteString
emptyFixture =
  ByteString.pack "{\"source\":\"broker\",\"retrieved_at\":\"2026-10-21T09:30:00Z\",\"positions\":[]}"

{-# LANGUAGE OverloadedStrings #-}

-- | Markdown rendering for portfolio snapshots; no calculations occur here.
module Seby.Reporting.PortfolioMarkdown (
  renderPortfolioMarkdown,
) where

import Data.Ratio (Ratio)
import Data.Text (Text)
import Data.Text qualified as Text
import Seby.Analysis.Portfolio (CurrencySummary (..), PositionSummary (..))
import Seby.Domain.Money (Currency (..), Money, moneyCurrency, toPaise)
import Seby.Domain.Types (
  provenanceDigest,
  provenanceRetrievedAt,
  provenanceSource,
  provenanceUri,
  unSourceId,
  unSymbol,
 )

renderPortfolioMarkdown :: [CurrencySummary] -> Text
renderPortfolioMarkdown summaries = Text.unlines ("# Seby Portfolio Snapshot" : "" : concatMap renderCurrency summaries)

renderCurrency :: CurrencySummary -> [Text]
renderCurrency summary =
  [ "## " <> currencyText (summaryCurrency summary)
  , ""
  , "| Symbol | Quantity | Average cost | Market price | Price date | Cost basis | Market value | Unrealized gain | Return on cost | Weight | Source | Retrieved at | Digest |"
  , "| --- | ---: | ---: | ---: | --- | ---: | ---: | ---: | ---: | ---: | --- | --- | --- |"
  ]
    <> fmap renderPosition (summaryPositions summary)
    <> [ ""
       , "**Totals** — Cost basis: "
           <> formatMoney (summaryTotalCostBasis summary)
           <> "; market value: "
           <> formatMoney (summaryTotalMarketValue summary)
           <> "; unrealized gain: "
           <> formatMoney (summaryTotalUnrealizedGain summary)
       , ""
       ]

renderPosition :: PositionSummary -> Text
renderPosition row =
  "| "
    <> escape (unSymbol (summarySymbol row))
    <> " | "
    <> formatRational (summaryQuantity row) 4
    <> " | "
    <> formatMoney (summaryAverageCost row)
    <> " | "
    <> formatMoney (summaryMarketPrice row)
    <> " | "
    <> Text.pack (show (summaryPriceDate row))
    <> " | "
    <> formatMoney (summaryCostBasis row)
    <> " | "
    <> formatMoney (summaryMarketValue row)
    <> " | "
    <> formatMoney (summaryUnrealizedGain row)
    <> " | "
    <> formatPercent (summaryReturnOnCost row)
    <> " | "
    <> formatPercent (summaryPortfolioWeight row)
    <> " | "
    <> sourceText row
    <> " | "
    <> Text.pack (show (provenanceRetrievedAt (summaryPriceProvenance row)))
    <> " | `sha256:"
    <> escape (provenanceDigest (summaryPriceProvenance row))
    <> "` |"

sourceText :: PositionSummary -> Text
sourceText row = case provenanceUri provenance of
  Nothing -> escape source
  Just uri -> "[" <> escape source <> "](" <> escape uri <> ")"
  where
    provenance = summaryPriceProvenance row
    source = unSourceId (provenanceSource provenance)

formatMoney :: Money -> Text
formatMoney amount = currencyText (moneyCurrency amount) <> " " <> formatMinorUnits (toPaise amount)

formatMinorUnits :: Integer -> Text
formatMinorUnits amount =
  (if amount < 0 then "-" else "")
    <> Text.pack (show (abs amount `div` 100))
    <> "."
    <> twoDigits (abs amount `mod` 100)

formatPercent :: Ratio Integer -> Text
formatPercent ratio = formatRational (ratio * 100) 2 <> "%"

formatRational :: Ratio Integer -> Int -> Text
formatRational ratio decimals =
  let scale = 10 ^ decimals
      scaled = round (ratio * fromIntegral scale) :: Integer
      absolute = abs scaled
      whole = absolute `div` scale
      fractional = absolute `mod` scale
   in (if scaled < 0 then "-" else "")
        <> Text.pack (show whole)
        <> if decimals == 0
          then ""
          else "." <> Text.justifyRight decimals '0' (Text.pack (show fractional))

twoDigits :: Integer -> Text
twoDigits value = Text.justifyRight 2 '0' (Text.pack (show value))

currencyText :: Currency -> Text
currencyText currency = case currency of
  INR -> "INR"
  USD -> "USD"
  EUR -> "EUR"

escape :: Text -> Text
escape =
  Text.replace "|" "\\|"
    . Text.replace "(" "\\("
    . Text.replace ")" "\\)"
    . Text.replace "[" "\\["
    . Text.replace "]" "\\]"
    . Text.replace "*" "\\*"
    . Text.replace "_" "\\_"
    . Text.replace ">" "\\>"
    . Text.replace "<" "\\<"
    . Text.replace "\n" " "
    . Text.replace "\r" " "
    . Text.replace "\\" "\\\\"

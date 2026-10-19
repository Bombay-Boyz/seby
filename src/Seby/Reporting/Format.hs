{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Shared financial labels and values for terminal and Markdown renderers.
module Seby.Reporting.Format (
  observationName,
  observationValue,
  observationPeriod,
  observationSources,
  issueText,
  basisText,
) where

import Data.Char (isControl)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Seby.Domain.Money (Currency (..), Money, moneyCurrency, toPaise)
import Seby.Domain.Period (
  FiscalPeriod (..),
  FiscalQuarter (..),
  dateRangeEnd,
  dateRangeStart,
  unFiscalYear,
 )
import Seby.Domain.Types (
  DataIssue (..),
  Days (..),
  Metric (..),
  Multiple,
  Provenance (..),
  Rate,
  SourceId (..),
  StatementBasis (..),
  unMultiple,
  unRate,
 )
import Seby.Reporting.Model (MetricObservation (..))

observationName :: MetricObservation -> Text
observationName (MetricObservation metric _ _ _) = metricName metric

observationValue :: MetricObservation -> Text
observationValue (MetricObservation metric value _ _) = metricValue metric value

observationPeriod :: MetricObservation -> Text
observationPeriod (MetricObservation _ _ period _) = periodText period

observationSources :: MetricObservation -> Text
observationSources (MetricObservation _ _ _ provenance) =
  Text.intercalate ", " (sourceText <$> NonEmpty.toList provenance)

metricName :: Metric a -> Text
metricName metric = case metric of
  Revenue -> "Revenue"
  OperatingProfit -> "Operating profit"
  NetProfit -> "Net profit"
  OperatingCashFlow -> "Operating cash flow"
  CapitalExpenditure -> "Capital expenditure"
  FreeCashFlow -> "Free cash flow"
  GrossMargin -> "Gross margin"
  OperatingMargin -> "Operating margin"
  NetMargin -> "Net margin"
  ReturnOnEquity -> "Return on equity"
  ReturnOnCapital -> "Return on capital"
  RevenueCAGR -> "Revenue CAGR"
  ProfitCAGR -> "Profit CAGR"
  PriceToEarnings -> "Price to earnings"
  PriceToBook -> "Price to book"
  EVToEBITDA -> "EV to EBITDA"
  DebtToEquity -> "Debt to equity"
  DaysSalesOut -> "Days sales outstanding"
  DaysInventoryOut -> "Days inventory outstanding"
  CashConversionCyc -> "Cash conversion cycle"

metricValue :: Metric a -> a -> Text
metricValue metric value = case metric of
  Revenue -> moneyText value
  OperatingProfit -> moneyText value
  NetProfit -> moneyText value
  OperatingCashFlow -> moneyText value
  CapitalExpenditure -> moneyText value
  FreeCashFlow -> moneyText value
  GrossMargin -> rateText value
  OperatingMargin -> rateText value
  NetMargin -> rateText value
  ReturnOnEquity -> rateText value
  ReturnOnCapital -> rateText value
  RevenueCAGR -> rateText value
  ProfitCAGR -> rateText value
  PriceToEarnings -> multipleText value
  PriceToBook -> multipleText value
  EVToEBITDA -> multipleText value
  DebtToEquity -> multipleText value
  DaysSalesOut -> daysText value
  DaysInventoryOut -> daysText value
  CashConversionCyc -> daysText value

moneyText :: Money -> Text
moneyText amount =
  let minor = toPaise amount
      absoluteMinor = abs minor
      (major, minorPart) = absoluteMinor `quotRem` 100
      sign = if minor < 0 then "-" else ""
      symbol = case moneyCurrency amount of
        INR -> "₹"
        USD -> "$"
        EUR -> "€"
   in sign
        <> symbol
        <> Text.pack (show major)
        <> "."
        <> padTwo minorPart
        <> " "
        <> currencyCode (moneyCurrency amount)

currencyCode :: Currency -> Text
currencyCode currency = case currency of
  INR -> "INR"
  USD -> "USD"
  EUR -> "EUR"

padTwo :: Integer -> Text
padTwo value
  | value < 10 = "0" <> Text.pack (show value)
  | otherwise = Text.pack (show value)

rateText :: Rate -> Text
rateText = (<> "%") . Text.pack . show . (* 100) . unRate

multipleText :: Multiple -> Text
multipleText = (<> "x") . Text.pack . show . unMultiple

daysText :: Days -> Text
daysText (Days count) = Text.pack (show count) <> " days"

periodText :: FiscalPeriod -> Text
periodText period = case period of
  AnnualPeriod fiscalYear -> "FY " <> Text.pack (show (unFiscalYear fiscalYear))
  QuarterlyPeriod fiscalYear quarter ->
    "Q" <> quarterText quarter <> " FY " <> Text.pack (show (unFiscalYear fiscalYear))
  CustomPeriod range ->
    Text.pack (show (dateRangeStart range)) <> " to " <> Text.pack (show (dateRangeEnd range))

quarterText :: FiscalQuarter -> Text
quarterText quarter = case quarter of
  Q1 -> "1"
  Q2 -> "2"
  Q3 -> "3"
  Q4 -> "4"

sourceText :: Provenance -> Text
sourceText source =
  let SourceId sourceName = provenanceSource source
      uriText = maybe "" (" — " <>) (provenanceUri source)
      publicationText = maybe "" (\time -> "; published " <> timeText time) (provenancePublishedAt source)
      digestText = singleLine (provenanceDigest source)
   in singleLine sourceName
        <> singleLine uriText
        <> " (retrieved "
        <> timeText (provenanceRetrievedAt source)
        <> publicationText
        <> "; digest "
        <> digestText
        <> ")"

timeText :: UTCTime -> Text
timeText = Text.pack . show

issueText :: DataIssue -> Text
issueText issue = case issue of
  NotDisclosed -> "Not disclosed"
  NotRetrieved -> "Not retrieved"
  NotApplicable -> "Not applicable"
  NotMeaningful reason -> "Not meaningful: " <> singleLine reason
  InvalidInput reason -> "Invalid input: " <> singleLine reason
  NonComparable reason -> "Non-comparable: " <> singleLine reason
  StaleObservation timestamp -> "Stale observation (last observed " <> timeText timestamp <> ")"
  ConflictingSources sources ->
    "Conflicting sources: " <> Text.intercalate ", " (sourceIdText <$> NonEmpty.toList sources)

sourceIdText :: SourceId -> Text
sourceIdText (SourceId value) = singleLine value

basisText :: StatementBasis -> Text
basisText basis = case basis of
  Standalone -> "Standalone"
  Consolidated -> "Consolidated"

singleLine :: Text -> Text
singleLine = Text.map (\character -> if isControl character then ' ' else character)

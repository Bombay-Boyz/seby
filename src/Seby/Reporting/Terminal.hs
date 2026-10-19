{-# LANGUAGE OverloadedStrings #-}

-- | Plain-text renderer for terminal analysis reports.
module Seby.Reporting.Terminal (
  renderTerminal,
) where

import Data.Char (isControl)
import Data.Text (Text)
import Data.Text qualified as Text
import Seby.Domain.Types (DataIssue, Symbol (..))
import Seby.Reporting.Format (
  basisText,
  issueText,
  observationName,
  observationPeriod,
  observationSources,
  observationValue,
 )
import Seby.Reporting.Model (
  MetricObservation,
  Report,
  companyName,
  companySymbol,
  reportBasis,
  reportCompany,
  reportDate,
  reportIssues,
  reportMetrics,
 )

-- | Render the canonical report without recalculating any financial value.
renderTerminal :: Report -> Text
renderTerminal report =
  Text.unlines
    ( [ "Seby Analysis Report"
      , "Company: " <> singleLine (companyName identity) <> " (" <> singleLine symbolText <> ")"
      , "Analysis date: " <> Text.pack (show (reportDate report))
      , "Statement basis: " <> basisText (reportBasis report)
      , ""
      , "Metrics"
      ]
        <> fmap renderMetric (reportMetrics report)
        <> issueLines (reportIssues report)
    )
  where
    identity = reportCompany report
    symbolText = unSymbol (companySymbol identity)

renderMetric :: MetricObservation -> Text
renderMetric metric =
  "  "
    <> observationName metric
    <> ": "
    <> observationValue metric
    <> " ("
    <> observationPeriod metric
    <> "; source: "
    <> observationSources metric
    <> ")"

issueLines :: [DataIssue] -> [Text]
issueLines [] = []
issueLines issues = "" : "Data quality" : fmap ("  - " <>) (issueText <$> issues)

singleLine :: Text -> Text
singleLine = Text.map (\character -> if isControl character then ' ' else character)

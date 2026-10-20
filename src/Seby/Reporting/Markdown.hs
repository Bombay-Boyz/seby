{-# LANGUAGE OverloadedStrings #-}

-- | Markdown renderer for the canonical analysis report.
module Seby.Reporting.Markdown (
  renderMarkdown,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Seby.Domain.Types (DataIssue, unCompanyId, unSymbol)
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
  companyId,
  companyName,
  companySymbol,
  reportBasis,
  reportCompany,
  reportDate,
  reportIssues,
  reportMetrics,
  reportPhase6Notes,
 )

-- | Render the shared report model as Markdown; no calculations occur here.
renderMarkdown :: Report -> Text
renderMarkdown report =
  Text.unlines
    ( [ "# Seby Analysis Report"
      , ""
      , "- **Company:** " <> escapeMarkdown (companyName identity) <> " (" <> escapeMarkdown ticker <> ")"
      , "- **Company ID:** " <> escapeMarkdown identifier
      , "- **Analysis date:** " <> Text.pack (show (reportDate report))
      , "- **Statement basis:** " <> basisText (reportBasis report)
      , ""
      , "## Metrics"
      , ""
      , "| Metric | Value | Period | Source |"
      , "| --- | --- | --- | --- |"
      ]
        <> fmap renderMetric (reportMetrics report)
        <> phase6Lines (reportPhase6Notes report)
        <> issueLines (reportIssues report)
    )
  where
    identity = reportCompany report
    ticker = unSymbol (companySymbol identity)
    identifier = unCompanyId (companyId identity)

renderMetric :: MetricObservation -> Text
renderMetric metric =
  "| "
    <> escapeMarkdown (observationName metric)
    <> " | "
    <> escapeMarkdown (observationValue metric)
    <> " | "
    <> escapeMarkdown (observationPeriod metric)
    <> " | "
    <> escapeMarkdown (observationSources metric)
    <> " |"

phase6Lines :: [Text] -> [Text]
phase6Lines [] = []
phase6Lines notes =
  ""
    : "## Corporate actions, segments, and review signals"
    : ""
    : fmap ("- " <>) (escapeMarkdown <$> notes)

issueLines :: [DataIssue] -> [Text]
issueLines [] = []
issueLines issues =
  "" : "## Data quality" : "" : fmap ("- " <>) (escapeMarkdown . issueText <$> issues)

escapeMarkdown :: Text -> Text
escapeMarkdown =
  Text.replace "\r" " "
    . Text.replace "\n" " "
    . Text.replace ">" "\\>"
    . Text.replace "<" "\\<"
    . Text.replace "]" "\\]"
    . Text.replace "[" "\\["
    . Text.replace "_" "\\_"
    . Text.replace "*" "\\*"
    . Text.replace "|" "\\|"
    . Text.replace "\\" "\\\\"

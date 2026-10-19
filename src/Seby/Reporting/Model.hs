{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Canonical, source-linked report model shared by all renderers.
module Seby.Reporting.Model (
  CompanyIdentity,
  mkCompanyIdentity,
  companyId,
  companySymbol,
  companyName,
  Report,
  mkReport,
  reportCompany,
  reportDate,
  reportBasis,
  reportMetrics,
  reportIssues,
  MetricObservation (..),
) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Calendar (Day)
import Seby.Domain.Period (FiscalPeriod)
import Seby.Domain.Types (
  CompanyId,
  DataIssue,
  Metric,
  Provenance,
  StatementBasis,
  Symbol,
 )

-- | Company identity keeps the stable entity identifier separate from its ticker.
data CompanyIdentity = CompanyIdentity !CompanyId !Symbol !Text
  deriving (Eq, Show)

-- | Company identity with a non-blank display name.
mkCompanyIdentity :: CompanyId -> Symbol -> Text -> Either Text CompanyIdentity
mkCompanyIdentity identifier ticker name
  | isBlank name = Left "Company name must contain a non-whitespace character"
  | otherwise = Right (CompanyIdentity identifier ticker name)
  where
    isBlank = Text.null . Text.strip

companyId :: CompanyIdentity -> CompanyId
companyId (CompanyIdentity identifier _ _) = identifier

companySymbol :: CompanyIdentity -> Symbol
companySymbol (CompanyIdentity _ ticker _) = ticker

companyName :: CompanyIdentity -> Text
companyName (CompanyIdentity _ _ name) = name

-- | A metric observation carries its typed value, reporting period, and non-empty lineage.
data MetricObservation where
  MetricObservation :: Metric a -> a -> FiscalPeriod -> NonEmpty Provenance -> MetricObservation

-- | One immutable, render-ready report snapshot.
data Report = Report !CompanyIdentity !Day !StatementBasis ![MetricObservation] ![DataIssue]

mkReport ::
  CompanyIdentity ->
  Day ->
  StatementBasis ->
  [MetricObservation] ->
  [DataIssue] ->
  Report
mkReport = Report

reportCompany :: Report -> CompanyIdentity
reportCompany (Report identity _ _ _ _) = identity

reportDate :: Report -> Day
reportDate (Report _ date _ _ _) = date

reportBasis :: Report -> StatementBasis
reportBasis (Report _ _ basis _ _) = basis

reportMetrics :: Report -> [MetricObservation]
reportMetrics (Report _ _ _ observations _) = observations

reportIssues :: Report -> [DataIssue]
reportIssues (Report _ _ _ _ issues) = issues

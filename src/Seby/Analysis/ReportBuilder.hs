{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Build a source-linked fundamentals report from persisted canonical facts.
Only metrics with unambiguous, dimension-compatible inputs are calculated.
-}
module Seby.Analysis.ReportBuilder (
  buildCompanyReport,
) where

import Data.List (nub, sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day)
import Seby.Domain.Money (Currency, Money, mkMoney)
import Seby.Domain.Period (FiscalPeriod (..), mkDateRange)
import Seby.Domain.Types (
  CompanyId,
  DataIssue (..),
  Metric (..),
  PeriodType,
  Provenance (..),
  Rate,
  StatementBasis (..),
  Symbol (..),
 )
import Seby.Metrics qualified as Metrics
import Seby.Persistence.Types (CanonicalFact (..))
import Seby.Reporting.Format (observationName, observationPeriod)
import Seby.Reporting.Model (
  MetricObservation (..),
  Report,
  mkCompanyIdentity,
  mkReportWithPhase6Notes,
 )

data PeriodKey = PeriodKey !Day !Day !PeriodType !Text !Currency
  deriving (Eq, Ord, Show)

buildCompanyReport ::
  Day ->
  CompanyId ->
  Symbol ->
  Text ->
  StatementBasis ->
  Text ->
  [CanonicalFact] ->
  Either Text Report
buildCompanyReport reportDate identifier ticker companyDisplayName basis scope allFacts = do
  identity <- mkCompanyIdentity identifier ticker companyDisplayName
  let facts =
        filter
          (\fact -> canonicalStatementBasis fact == basis && canonicalScope fact == scope)
          allFacts
      periodGroups = Map.fromListWith (<>) [(periodKey fact, [fact]) | fact <- facts]
      rawRows = concatMap (uncurry rawObservations) (Map.toAscList periodGroups)
      derivedResults = fmap (uncurry derivedObservations) (Map.toAscList periodGroups)
      derivedRows = concatMap fst derivedResults
      derivedIssues = concatMap snd derivedResults
      rows = sortOn observationSortKey (rawRows <> derivedRows)
      issues = conflictIssues facts <> derivedIssues
      unsupported = unsupportedConcepts facts
  if null facts
    then
      Left
        ( "No persisted facts found for "
            <> unSymbol ticker
            <> " with basis "
            <> basisLabel basis
            <> " and scope "
            <> scope
            <> ". Import sourced statements with `seby facts import FILE` first."
        )
    else
      if null rows
        then Left "Imported facts contain no concepts supported by the report engine"
        else
          Right $
            mkReportWithPhase6Notes
              identity
              reportDate
              basis
              rows
              issues
              ( [ "Scope: " <> scope
                , "Only available sourced facts and metrics with complete compatible inputs are shown."
                ]
                  <> if null unsupported then [] else ["Unrendered concepts: " <> Text.intercalate ", " unsupported]
              )

periodKey :: CanonicalFact -> PeriodKey
periodKey fact =
  PeriodKey
    (canonicalPeriodStart fact)
    (canonicalPeriodEnd fact)
    (canonicalPeriodType fact)
    (canonicalScope fact)
    (canonicalCurrency fact)

rawObservations :: PeriodKey -> [CanonicalFact] -> [MetricObservation]
rawObservations key facts = concatMap (makeMetricRows key) (Map.toAscList grouped)
  where
    recognized =
      [(canonicalConcept fact, fact) | fact <- facts, isJust (metricForConcept (canonicalConcept fact))]
    grouped = Map.fromListWith (<>) [(concept, [fact]) | (concept, fact) <- recognized]

makeMetricRows :: PeriodKey -> (Text, [CanonicalFact]) -> [MetricObservation]
makeMetricRows key (concept, facts) = case (metricForConcept concept, toFiscalPeriod key) of
  (Nothing, _) -> []
  (_, Left _) -> []
  (Just metric, Right period) ->
    [ MetricObservation metric (mkMoney currency amount) period provenance
    | (amount, amountFacts) <-
        Map.toAscList (Map.fromListWith (<>) [(canonicalAmountMinor fact, [fact]) | fact <- facts])
    , Just provenance <- [nonEmptyProvenance amountFacts]
    ]
  where
    currency = case key of
      PeriodKey _ _ _ _ currentCurrency -> currentCurrency

metricForConcept :: Text -> Maybe (Metric Money)
metricForConcept concept = case concept of
  "revenue" -> Just Revenue
  "operating_profit" -> Just OperatingProfit
  "net_profit" -> Just NetProfit
  "operating_cash_flow" -> Just OperatingCashFlow
  "capital_expenditure" -> Just CapitalExpenditure
  _ -> Nothing

derivedObservations :: PeriodKey -> [CanonicalFact] -> ([MetricObservation], [DataIssue])
derivedObservations key facts = case toFiscalPeriod key of
  Left _ -> ([], [InvalidInput "A report fact has a reversed reporting period"])
  Right period ->
    let periodFacts = Map.fromListWith (<>) [(canonicalConcept fact, [fact]) | fact <- facts]
        (fcfRows, fcfIssues) =
          deriveMoneyMetric
            "operating_cash_flow"
            "capital_expenditure"
            FreeCashFlow
            Metrics.freeCashFlow
            period
            periodFacts
        (grossRows, grossIssues) = deriveRateMetric "gross_profit" "revenue" GrossMargin Metrics.grossMargin period periodFacts
        (operatingRows, operatingIssues) =
          deriveRateMetric
            "operating_profit"
            "revenue"
            OperatingMargin
            Metrics.operatingMargin
            period
            periodFacts
        (netRows, netIssues) = deriveRateMetric "net_profit" "revenue" NetMargin Metrics.netMargin period periodFacts
     in ( fcfRows <> grossRows <> operatingRows <> netRows
        , fcfIssues <> grossIssues <> operatingIssues <> netIssues
        )

deriveMoneyMetric ::
  Text ->
  Text ->
  Metric Money ->
  (Money -> Money -> Either Metrics.MetricError Money) ->
  FiscalPeriod ->
  Map.Map Text [CanonicalFact] ->
  ([MetricObservation], [DataIssue])
deriveMoneyMetric firstConcept secondConcept metric calculate period facts =
  case (uniqueAmount firstConcept facts, uniqueAmount secondConcept facts) of
    (Just (firstMoney, firstSources), Just (secondMoney, secondSources)) ->
      case calculate firstMoney secondMoney of
        Left calculationError -> ([], [NotMeaningful (Text.pack (show calculationError))])
        Right value -> makeDerivedRow metric value period (firstSources <> secondSources)
    _ -> ([], [])

deriveRateMetric ::
  Text ->
  Text ->
  Metric Rate ->
  (Money -> Money -> Either Metrics.MetricError Rate) ->
  FiscalPeriod ->
  Map.Map Text [CanonicalFact] ->
  ([MetricObservation], [DataIssue])
deriveRateMetric numeratorConcept denominatorConcept metric calculate period facts =
  case (uniqueAmount numeratorConcept facts, uniqueAmount denominatorConcept facts) of
    (Just (numeratorMoney, numeratorSources), Just (denominatorMoney, denominatorSources)) ->
      case calculate numeratorMoney denominatorMoney of
        Left calculationError -> ([], [NotMeaningful (Text.pack (show calculationError))])
        Right value -> makeDerivedRow metric value period (numeratorSources <> denominatorSources)
    _ -> ([], [])

uniqueAmount :: Text -> Map.Map Text [CanonicalFact] -> Maybe (Money, NonEmpty Provenance)
uniqueAmount concept facts = do
  conceptFacts <- Map.lookup concept facts
  let amounts = nub (canonicalAmountMinor <$> conceptFacts)
  amount <- case amounts of
    [onlyAmount] -> Just onlyAmount
    _ -> Nothing
  provenance <- nonEmptyProvenance conceptFacts
  case conceptFacts of
    firstFact : _ -> Just (mkMoney (canonicalCurrency firstFact) amount, provenance)
    [] -> Nothing

makeDerivedRow ::
  Metric a -> a -> FiscalPeriod -> NonEmpty Provenance -> ([MetricObservation], [DataIssue])
makeDerivedRow metric value period provenance =
  case NonEmpty.nonEmpty (nub (NonEmpty.toList provenance)) of
    Nothing -> ([], [])
    Just uniqueProvenance -> ([MetricObservation metric value period uniqueProvenance], [])

toFiscalPeriod :: PeriodKey -> Either Text FiscalPeriod
toFiscalPeriod (PeriodKey start end _ _ _) = case mkDateRange start end of
  Left _ -> Left "reversed date range"
  Right dateRange -> Right (CustomPeriod dateRange)

nonEmptyProvenance :: [CanonicalFact] -> Maybe (NonEmpty Provenance)
nonEmptyProvenance facts = NonEmpty.nonEmpty (nub (canonicalProvenance <$> facts))

conflictIssues :: [CanonicalFact] -> [DataIssue]
conflictIssues facts = concatMap issueForGroup (Map.elems grouped)
  where
    grouped = Map.fromListWith (<>) [(conflictKey fact, [fact]) | fact <- facts]
    issueForGroup group
      | length (nub (canonicalAmountMinor <$> group)) <= 1 = []
      | otherwise = case NonEmpty.nonEmpty (nub (provenanceSource . canonicalProvenance <$> group)) of
          Nothing -> []
          Just sources -> [ConflictingSources sources]

conflictKey :: CanonicalFact -> (Text, PeriodKey)
conflictKey fact = (canonicalConcept fact, periodKey fact)

unsupportedConcepts :: [CanonicalFact] -> [Text]
unsupportedConcepts = nub . sortOn id . filter (not . isSupported) . fmap canonicalConcept
  where
    isSupported concept = isJust (metricForConcept concept) || concept == "gross_profit"

basisLabel :: StatementBasis -> Text
basisLabel basis = case basis of
  Consolidated -> "consolidated"
  Standalone -> "standalone"

observationSortKey :: MetricObservation -> (Text, Text)
observationSortKey observation = (observationName observation, observationPeriod observation)

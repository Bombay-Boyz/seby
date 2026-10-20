{-# LANGUAGE OverloadedStrings #-}

module Seby.Phase6Spec (runTests) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isNothing)
import Data.Ratio ((%))
import Data.Time (Day, UTCTime (..), fromGregorian, secondsToDiffTime)
import Seby.Analysis.CorporateActions
import Seby.Analysis.Forensics
import Seby.Analysis.Segments
import Seby.Domain.Money (Currency (..), fromRupees, mkMoney)
import Seby.Domain.Period (FiscalPeriod (..), mkFiscalYear)
import Seby.Domain.Types (
  CompanyId (..),
  ConsolidationScope (..),
  Provenance (..),
  SourceId (..),
  mkMultiple,
  mkPositiveYears,
  mkRate,
  unRate,
 )
import Seby.Market.Prices (mkDailyPrice)

runTests :: IO ()
runTests = do
  runCorporateActionTests
  runSegmentTests
  runForensicTests
  putStrLn
    "Phase 6 tests passed (action history, segment reconciliation, and evidence-based signals)."

runCorporateActionTests :: IO ()
runCorporateActionTests = do
  let company = CompanyId "CO"
      priceDate = fromGregorian 2023 1 1
      splitDate = fromGregorian 2024 7 1
      bonusDate = fromGregorian 2025 6 1
      rightsDate = fromGregorian 2025 11 1
      splitProvenance = provenance (Just (utc (fromGregorian 2024 6 1)))
      bonusProvenance = provenance (Just (utc (fromGregorian 2025 5 1)))
      rightsProvenance = provenance (Just (utc (fromGregorian 2025 10 1)))
  reportedPrice <-
    expectRight "valid daily share price" (mkDailyPrice INR priceDate 400 400 400 400 100)
  split <-
    expectRight
      "split action"
      (mkCorporateAction "split-2-for-1" company splitDate (ShareSplit (2 % 1)) splitProvenance)
  bonus <-
    expectRight
      "bonus action"
      (mkCorporateAction "bonus-1-for-1" company bonusDate (BonusIssue (1 % 1)) bonusProvenance)
  rights <-
    expectRight
      "rights action"
      (mkCorporateAction "rights" company rightsDate RightsIssue rightsProvenance)
  ledger <- expectRight "sorted action ledger" (mkActionLedger [rights, bonus, split])
  assert
    "ledger sorted by effective date"
    (map corporateActionId (ledgerActions ledger) == ["split-2-for-1", "bonus-1-for-1", "rights"])
  assert "duplicate action IDs rejected" (isDuplicateAction (mkActionLedger [split, split]))
  assert
    "zero split ratio rejected"
    (isInvalidRatio (mkCorporateAction "bad" company splitDate (ShareSplit (0 % 1)) splitProvenance))

  let reported = viewHistoricalSharePrice AsReported company reportedPrice ledger
      comparable =
        viewHistoricalSharePrice (ComparableThrough (fromGregorian 2026 1 1)) company reportedPrice ledger
      beforeEffective =
        viewHistoricalSharePrice
          (PointInTimeAt (utc (fromGregorian 2024 6 15)))
          company
          reportedPrice
          ledger
      afterBonus =
        viewHistoricalSharePrice (PointInTimeAt (utc (fromGregorian 2025 6 2))) company reportedPrice ledger
      acrossRights =
        viewHistoricalSharePrice (ComparableThrough (fromGregorian 2026 1 1)) company reportedPrice ledger
      otherCompany =
        viewHistoricalSharePrice
          (ComparableThrough (fromGregorian 2026 1 1))
          (CompanyId "OTHER")
          reportedPrice
          ledger
  assert
    "reported view preserves unadjusted value"
    (comparableSharePrice reported == Just 400 && null (appliedSharePriceAdjustments reported))
  assert
    "split and bonus apply exact inverse factors"
    (isNothing (comparableSharePrice comparable) && length (appliedSharePriceAdjustments comparable) == 2)
  assert
    "point-in-time before effective date does not adjust"
    (comparableSharePrice beforeEffective == Just 400)
  assert
    "point-in-time after known split and bonus adjusts"
    (comparableSharePrice afterBonus == Just 100)
  assert
    "rights issue marks series non-comparable"
    ( isNothing (comparableSharePrice acrossRights)
        && sharePriceComparabilityIssues acrossRights == [RightsIssueNeedsManualAdjustment "rights"]
    )
  assert
    "different companies do not share corporate actions"
    (comparableSharePrice otherCompany == Just 400)
  assert
    "action is unavailable before publication"
    (null (actionsKnownAt (utc (fromGregorian 2024 5 1)) ledger))

runSegmentTests :: IO ()
runSegmentTests = do
  fiscalYear <- expectRight "segment fiscal year" (mkFiscalYear 2025)
  north <-
    expectRight "north segment" (mkSegmentRevenue "North" (fromRupees 600) (provenance Nothing :| []))
  south <-
    expectRight "south segment" (mkSegmentRevenue "South" (fromRupees 400) (provenance Nothing :| []))
  let company = CompanyId "CO"
      period = AnnualPeriod fiscalYear
      segments = north :| [south]
  complete <-
    expectRight
      "complete segment analysis"
      (analyzeSegments company period GroupScope (fromRupees 1000) CompleteDisclosure segments)
  assert
    "complete segments reconcile exactly"
    (segmentAnalysisReconciliationDifference complete == fromRupees 0)
  assert
    "segment shares retain proportions"
    (map segmentShareOfGroupRevenue (toList (segmentAnalysisShares complete)) == [3 % 5, 2 % 5])
  assert
    "reconciled segment HHI is 0.52"
    (segmentAnalysisConcentration complete == HhiAvailable (13 % 25))

  partial <-
    expectRight
      "partial segment analysis"
      (analyzeSegments company period GroupScope (fromRupees 1000) PartialDisclosure segments)
  assert
    "partial disclosure suppresses HHI"
    (segmentAnalysisConcentration partial == HhiUnavailablePartialDisclosure)
  mismatched <-
    expectRight
      "unreconciled segment analysis"
      (analyzeSegments company period GroupScope (fromRupees 1100) CompleteDisclosure segments)
  assert
    "unreconciled complete disclosure suppresses HHI"
    (segmentAnalysisConcentration mismatched == HhiUnavailableUnreconciled)
  lossSegment <-
    expectRight
      "negative segment retained"
      (mkSegmentRevenue "Other" (fromRupees (-100)) (provenance Nothing :| []))
  positiveSegment <-
    expectRight
      "offsetting segment retained"
      (mkSegmentRevenue "Core" (fromRupees 1100) (provenance Nothing :| []))
  negativeMix <-
    expectRight
      "negative segment totals retained"
      ( analyzeSegments
          company
          period
          GroupScope
          (fromRupees 1000)
          CompleteDisclosure
          (lossSegment :| [positiveSegment])
      )
  let firstSegmentShare = case segmentAnalysisShares negativeMix of
        share :| _ -> Just share
  assert "negative segment revenue disables HHI without clipping" $
    segmentAnalysisConcentration negativeMix == HhiUnavailableNegativeSegmentRevenue
      && maybe False ((== fromRupees (-100)) . segmentShareAmount) firstSegmentShare
  duplicate <-
    expectRight
      "duplicate fixture segment"
      (mkSegmentRevenue "North" (fromRupees 1) (provenance Nothing :| []))
  assert
    "duplicate segment labels rejected"
    ( isDuplicateSegment
        ( analyzeSegments
            company
            period
            GroupScope
            (fromRupees 1000)
            CompleteDisclosure
            (north :| [duplicate])
        )
    )
  foreignSegment <-
    expectRight
      "foreign-currency segment"
      (mkSegmentRevenue "Export" (mkMoney USD 100) (provenance Nothing :| []))
  assert
    "mixed currency segment input rejected"
    ( isSegmentCurrencyMismatch
        ( analyzeSegments
            company
            period
            GroupScope
            (fromRupees 1000)
            CompleteDisclosure
            (north :| [foreignSegment])
        )
    )

runForensicTests :: IO ()
runForensicTests = do
  threshold <- expectRight "cash conversion threshold" (mkMultiple 0.8)
  observations <-
    traverse
      makeCashEvidence
      [ (2022, 60, 100)
      , (2023, 70, 100)
      , (2024, 50, 100)
      , (2025, 120, 100)
      , (2026, 50, 100)
      , (2027, 60, 100)
      ]
  observationSet <- requireNonEmpty "cash conversion observations" observations
  flags <-
    expectRight
      "persistent cash conversion detector"
      (persistentCashConversionFlags 3 threshold observationSet)
  case flags of
    [PersistentCashConversionDivergence start end _ samples explanations] -> do
      assert
        "cash divergence captures only the persistent run"
        (start == fromGregorian 2022 12 31 && end == fromGregorian 2024 12 31 && length (toList samples) == 3)
      assert "cash divergence includes benign explanations" (length (toList explanations) >= 2)
    _ -> fail "expected one persistent cash-conversion investigation flag"
  onePeriod <- traverse makeCashEvidence [(2022, 60, 100), (2023, -20, -10), (2024, 60, 100)]
  onePeriodSet <- requireNonEmpty "short cash conversion observations" onePeriod
  noPersistentFlag <-
    expectRight
      "non-positive income breaks a run"
      (persistentCashConversionFlags 2 threshold onePeriodSet)
  assert "loss periods do not create a persistence flag" (null noPersistentFlag)
  reversedSet <- requireNonEmpty "reversed cash conversion observations" (reverse observations)
  assert
    "unordered periods rejected"
    (isUnorderedPeriods (persistentCashConversionFlags 2 threshold reversedSet))
  assert
    "single-period persistence window rejected"
    (isInvalidWindow (persistentCashConversionFlags 1 threshold observationSet))

  twoYears <- expectRight "two-year duration" (mkPositiveYears 2)
  excess <- expectRight "growth excess threshold" (mkRate 0.1)
  let start = fromGregorian 2022 3 31
      end = fromGregorian 2024 3 31
  growthEvidence <-
    expectRight "receivables growth evidence" $
      mkGrowthComparisonEvidence
        start
        end
        twoYears
        (fromRupees 1000)
        (fromRupees 1200)
        (fromRupees 100)
        (fromRupees 200)
        (provenance Nothing :| [])
  growthFlag <-
    expectRight "receivables divergence detector" (receivablesGrowthFlag excess growthEvidence)
  case growthFlag of
    Just
      (ReceivablesGrowingFasterThanRevenue _ _ receivablesGrowth revenueGrowth _ _ benignExplanations) -> do
        assert
          "receivables growth exceeds revenue growth"
          (unRate receivablesGrowth > unRate revenueGrowth + 0.1)
        assert
          "receivables signal has benign explanation"
          (not (null (toList benignExplanations)))
    _ -> fail "expected receivables-growth investigation flag"
  assert
    "invalid date range rejected"
    ( isInvalidEvidencePeriod
        ( mkGrowthComparisonEvidence
            end
            start
            twoYears
            (fromRupees 1)
            (fromRupees 2)
            (fromRupees 1)
            (fromRupees 2)
            (provenance Nothing :| [])
        )
    )

makeCashEvidence :: (Integer, Integer, Integer) -> IO CashConversionEvidence
makeCashEvidence (year, operatingCashFlow, netIncome) =
  expectRight "cash conversion fixture" $
    mkCashConversionEvidence
      (fromGregorian year 12 31)
      (fromRupees operatingCashFlow)
      (fromRupees netIncome)
      (provenance Nothing :| [])

provenance :: Maybe UTCTime -> Provenance
provenance publishedAt =
  Provenance
    (SourceId "ANNUAL_REPORT")
    Nothing
    (utc (fromGregorian 2026 1 1))
    publishedAt
    "fixture-digest"

utc :: Day -> UTCTime
utc day = UTCTime day (secondsToDiffTime 0)

toList :: NonEmpty a -> [a]
toList (first :| rest) = first : rest

requireNonEmpty :: String -> [a] -> IO (NonEmpty a)
requireNonEmpty _ (first : rest) = pure (first :| rest)
requireNonEmpty label [] = fail (label <> " unexpectedly empty")

expectRight :: (Show e) => String -> Either e a -> IO a
expectRight label result = case result of
  Right value -> pure value
  Left err -> fail (label <> ": " <> show err)

assert :: String -> Bool -> IO ()
assert label condition = if condition then pure () else fail ("failed: " <> label)

isDuplicateAction :: Either CorporateActionError ActionLedger -> Bool
isDuplicateAction (Left (DuplicateCorporateActionId _)) = True
isDuplicateAction _ = False

isInvalidRatio :: Either CorporateActionError CorporateAction -> Bool
isInvalidRatio (Left (InvalidCorporateActionRatio _)) = True
isInvalidRatio _ = False

isDuplicateSegment :: Either SegmentError SegmentAnalysis -> Bool
isDuplicateSegment (Left (DuplicateSegmentName _)) = True
isDuplicateSegment _ = False

isSegmentCurrencyMismatch :: Either SegmentError SegmentAnalysis -> Bool
isSegmentCurrencyMismatch (Left SegmentCurrencyMismatch) = True
isSegmentCurrencyMismatch _ = False

isUnorderedPeriods :: Either ForensicError [InvestigationFlag] -> Bool
isUnorderedPeriods (Left (UnorderedCashFlowPeriods _ _)) = True
isUnorderedPeriods _ = False

isInvalidWindow :: Either ForensicError [InvestigationFlag] -> Bool
isInvalidWindow (Left (InvalidPersistenceWindow _)) = True
isInvalidWindow _ = False

isInvalidEvidencePeriod :: Either ForensicError GrowthComparisonEvidence -> Bool
isInvalidEvidencePeriod (Left (InvalidEvidencePeriod _ _)) = True
isInvalidEvidencePeriod _ = False

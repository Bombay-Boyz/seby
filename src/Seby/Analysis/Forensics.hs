{-# LANGUAGE DerivingStrategies #-}

{- | Evidence-based accounting investigation signals.

Signals describe measured patterns and include the underlying observations and
source lineage. They are prompts for review, never findings of misconduct.
-}
module Seby.Analysis.Forensics (
  CashConversionEvidence,
  mkCashConversionEvidence,
  cashEvidencePeriodEnd,
  cashEvidenceOperatingCashFlow,
  cashEvidenceNetIncome,
  cashEvidenceProvenance,
  CashConversionSample (..),
  GrowthComparisonEvidence,
  mkGrowthComparisonEvidence,
  growthEvidenceStart,
  growthEvidenceEnd,
  growthEvidenceDuration,
  growthEvidenceOpeningRevenue,
  growthEvidenceClosingRevenue,
  growthEvidenceOpeningReceivables,
  growthEvidenceClosingReceivables,
  growthEvidenceProvenance,
  ForensicError (..),
  InvestigationFlag (..),
  persistentCashConversionFlags,
  receivablesGrowthFlag,
) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Ratio (Ratio)
import Data.Text (Text)
import Data.Time (Day)
import Seby.Domain.Money (Money, moneyCurrency, moneyRatio, moneySign)
import Seby.Domain.Types (Multiple, PositiveYears, Provenance, Rate, unMultiple, unRate)
import Seby.Metrics (MetricError, growthRate)

data CashConversionEvidence = CashConversionEvidence !Day !Money !Money !(NonEmpty Provenance)
  deriving stock (Eq, Show)

mkCashConversionEvidence ::
  Day -> Money -> Money -> NonEmpty Provenance -> Either ForensicError CashConversionEvidence
mkCashConversionEvidence periodEnd operatingCashFlow netIncome provenance
  | moneyCurrency operatingCashFlow /= moneyCurrency netIncome = Left ForensicCurrencyMismatch
  | otherwise = Right (CashConversionEvidence periodEnd operatingCashFlow netIncome provenance)

cashEvidencePeriodEnd :: CashConversionEvidence -> Day
cashEvidencePeriodEnd (CashConversionEvidence day _ _ _) = day

cashEvidenceOperatingCashFlow :: CashConversionEvidence -> Money
cashEvidenceOperatingCashFlow (CashConversionEvidence _ value _ _) = value

cashEvidenceNetIncome :: CashConversionEvidence -> Money
cashEvidenceNetIncome (CashConversionEvidence _ _ value _) = value

cashEvidenceProvenance :: CashConversionEvidence -> NonEmpty Provenance
cashEvidenceProvenance (CashConversionEvidence _ _ _ provenance) = provenance

data GrowthComparisonEvidence
  = GrowthComparisonEvidence !Day !Day !PositiveYears !Money !Money !Money !Money !(NonEmpty Provenance)
  deriving stock (Eq, Show)

mkGrowthComparisonEvidence ::
  Day ->
  Day ->
  PositiveYears ->
  Money ->
  Money ->
  Money ->
  Money ->
  NonEmpty Provenance ->
  Either ForensicError GrowthComparisonEvidence
mkGrowthComparisonEvidence start end duration openingRevenue closingRevenue openingReceivables closingReceivables provenance
  | end <= start = Left (InvalidEvidencePeriod start end)
  | any
      ((/= moneyCurrency openingRevenue) . moneyCurrency)
      [closingRevenue, openingReceivables, closingReceivables] =
      Left ForensicCurrencyMismatch
  | otherwise =
      Right
        ( GrowthComparisonEvidence
            start
            end
            duration
            openingRevenue
            closingRevenue
            openingReceivables
            closingReceivables
            provenance
        )

growthEvidenceStart :: GrowthComparisonEvidence -> Day
growthEvidenceStart (GrowthComparisonEvidence start _ _ _ _ _ _ _) = start

growthEvidenceEnd :: GrowthComparisonEvidence -> Day
growthEvidenceEnd (GrowthComparisonEvidence _ end _ _ _ _ _ _) = end

growthEvidenceDuration :: GrowthComparisonEvidence -> PositiveYears
growthEvidenceDuration (GrowthComparisonEvidence _ _ duration _ _ _ _ _) = duration

growthEvidenceOpeningRevenue :: GrowthComparisonEvidence -> Money
growthEvidenceOpeningRevenue (GrowthComparisonEvidence _ _ _ value _ _ _ _) = value

growthEvidenceClosingRevenue :: GrowthComparisonEvidence -> Money
growthEvidenceClosingRevenue (GrowthComparisonEvidence _ _ _ _ value _ _ _) = value

growthEvidenceOpeningReceivables :: GrowthComparisonEvidence -> Money
growthEvidenceOpeningReceivables (GrowthComparisonEvidence _ _ _ _ _ value _ _) = value

growthEvidenceClosingReceivables :: GrowthComparisonEvidence -> Money
growthEvidenceClosingReceivables (GrowthComparisonEvidence _ _ _ _ _ _ value _) = value

growthEvidenceProvenance :: GrowthComparisonEvidence -> NonEmpty Provenance
growthEvidenceProvenance (GrowthComparisonEvidence _ _ _ _ _ _ _ provenance) = provenance

data ForensicError
  = InvalidPersistenceWindow !Int
  | InvalidCashConversionThreshold !Double
  | InvalidGrowthThreshold !Double
  | InvalidEvidencePeriod !Day !Day
  | ForensicCurrencyMismatch
  | UnorderedCashFlowPeriods !Day !Day
  | ForensicCalculationFailure !MetricError
  deriving stock (Eq, Show)

data CashConversionSample = CashConversionSample
  { cashConversionSamplePeriod :: !Day
  , cashConversionSampleRatio :: !(Ratio Integer)
  , cashConversionSampleEvidence :: !CashConversionEvidence
  }
  deriving stock (Eq, Show)

data InvestigationFlag
  = PersistentCashConversionDivergence
      !Day
      !Day
      !Multiple
      !(NonEmpty CashConversionSample)
      !(NonEmpty Text)
  | ReceivablesGrowingFasterThanRevenue
      !Day
      !Day
      !Rate
      !Rate
      !Rate
      !GrowthComparisonEvidence
      !(NonEmpty Text)
  deriving stock (Eq, Show)

{- | Flag each consecutive run that meets the supplied minimum length and falls
below the analyst-selected operating-cash-flow/net-income threshold. Periods
with non-positive net income do not produce a ratio and break a run.
-}
persistentCashConversionFlags ::
  Int -> Multiple -> NonEmpty CashConversionEvidence -> Either ForensicError [InvestigationFlag]
persistentCashConversionFlags minimumPeriods threshold observations = do
  if minimumPeriods >= 2 then pure () else Left (InvalidPersistenceWindow minimumPeriods)
  if unMultiple threshold > 0
    then pure ()
    else Left (InvalidCashConversionThreshold (unMultiple threshold))
  validateCashPeriods observations
  samples <- traverse toSample (NonEmpty.toList observations)
  let qualifyingRuns = collectRuns (toRational (unMultiple threshold)) samples
  pure
    [ PersistentCashConversionDivergence
        (cashConversionSamplePeriod first)
        (cashConversionSamplePeriod final)
        threshold
        run
        ( "Temporary working-capital investment can depress operating cash flow"
            :| [ "Seasonality or payment timing can shift cash receipts between periods"
               , "Acquisitions or one-off tax payments may affect cash flow"
               ]
        )
    | run@(first :| _) <- qualifyingRuns
    , NonEmpty.length run >= minimumPeriods
    , let final = lastValue run
    ]

validateCashPeriods :: NonEmpty CashConversionEvidence -> Either ForensicError ()
validateCashPeriods (first :| rest) = go (cashEvidencePeriodEnd first) rest
  where
    currency = moneyCurrency (cashEvidenceOperatingCashFlow first)
    go _ [] = Right ()
    go previous (current : remaining)
      | moneyCurrency (cashEvidenceOperatingCashFlow current) /= currency = Left ForensicCurrencyMismatch
      | cashEvidencePeriodEnd current <= previous =
          Left (UnorderedCashFlowPeriods previous (cashEvidencePeriodEnd current))
      | otherwise = go (cashEvidencePeriodEnd current) remaining

toSample :: CashConversionEvidence -> Either ForensicError (Maybe CashConversionSample)
toSample evidence
  | moneySign (cashEvidenceNetIncome evidence) /= GT = Right Nothing
  | otherwise = case moneyRatio (cashEvidenceOperatingCashFlow evidence) (cashEvidenceNetIncome evidence) of
      Left _ -> Left ForensicCurrencyMismatch
      Right ratio -> Right (Just (CashConversionSample (cashEvidencePeriodEnd evidence) ratio evidence))

collectRuns :: Ratio Integer -> [Maybe CashConversionSample] -> [NonEmpty CashConversionSample]
collectRuns threshold = go []
  where
    go current [] = flush current
    go current (maybeSample : remaining) = case maybeSample of
      Just sample
        | cashConversionSampleRatio sample < threshold -> go (sample : current) remaining
      _ -> flush current <> go [] remaining
    flush current = case reverse current of
      [] -> []
      first : rest -> [first :| rest]

receivablesGrowthFlag ::
  Rate -> GrowthComparisonEvidence -> Either ForensicError (Maybe InvestigationFlag)
receivablesGrowthFlag
  excessThreshold
  evidence@( GrowthComparisonEvidence
               start
               end
               duration
               openingRevenue
               closingRevenue
               openingReceivables
               closingReceivables
               _
             ) = do
    if unRate excessThreshold >= 0
      then pure ()
      else Left (InvalidGrowthThreshold (unRate excessThreshold))
    revenueGrowth <- case growthRate duration openingRevenue closingRevenue of
      Left err -> Left (ForensicCalculationFailure err)
      Right value -> Right value
    receivablesGrowth <- case growthRate duration openingReceivables closingReceivables of
      Left err -> Left (ForensicCalculationFailure err)
      Right value -> Right value
    if unRate receivablesGrowth - unRate revenueGrowth > unRate excessThreshold
      then
        Right . Just $
          ReceivablesGrowingFasterThanRevenue
            start
            end
            receivablesGrowth
            revenueGrowth
            excessThreshold
            evidence
            ( "A change in customer mix or credit terms can increase receivables"
                :| [ "Large contracts may have billing and collection milestones"
                   , "Acquisitions or seasonality can affect period-end receivables"
                   ]
            )
      else Right Nothing

lastValue :: NonEmpty a -> a
lastValue (first :| rest) = foldl (\_ next -> next) first rest

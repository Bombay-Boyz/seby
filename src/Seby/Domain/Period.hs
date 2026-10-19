{-# LANGUAGE DerivingStrategies #-}

-- | Validated Indian fiscal periods and elapsed-time calculations.
module Seby.Domain.Period (
  FiscalYear,
  mkFiscalYear,
  unFiscalYear,
  FiscalQuarter (..),
  DateRange,
  mkDateRange,
  dateRangeStart,
  dateRangeEnd,
  FiscalPeriod (..),
  PeriodError (..),
  YearFraction,
  mkYearFraction,
  unYearFraction,
  diffDaysYearFraction,
  fiscalYearBounds,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Calendar (Day, diffDays, fromGregorian)

-- | Errors produced while validating a fiscal period or duration.
data PeriodError
  = InvalidFiscalYear !Integer
  | ReversedDateRange !Day !Day
  | ReversedDateInterval !Day !Day
  | NonFiniteYearFraction !Text
  | NegativeYearFraction !Text
  deriving stock (Eq, Show)

-- | Fiscal year labelled by its ending calendar year.
newtype FiscalYear = UnsafeFiscalYear Integer
  deriving stock (Eq, Ord, Show)

-- | Construct a positive fiscal year label.
mkFiscalYear :: Integer -> Either PeriodError FiscalYear
mkFiscalYear year
  | year < 1 = Left (InvalidFiscalYear year)
  | otherwise = Right (UnsafeFiscalYear year)

unFiscalYear :: FiscalYear -> Integer
unFiscalYear (UnsafeFiscalYear year) = year

-- | Fiscal quarter in the Indian financial calendar.
data FiscalQuarter = Q1 | Q2 | Q3 | Q4
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Inclusive, validated custom reporting date interval.
data DateRange = DateRange !Day !Day
  deriving stock (Eq, Ord, Show)

-- | Construct a custom interval whose end is on or after its start.
mkDateRange :: Day -> Day -> Either PeriodError DateRange
mkDateRange start end
  | end < start = Left (ReversedDateRange start end)
  | otherwise = Right (DateRange start end)

dateRangeStart :: DateRange -> Day
dateRangeStart (DateRange start _) = start

dateRangeEnd :: DateRange -> Day
dateRangeEnd (DateRange _ end) = end

-- | Annual, quarterly, or explicitly bounded custom fiscal period.
data FiscalPeriod
  = AnnualPeriod !FiscalYear
  | QuarterlyPeriod !FiscalYear !FiscalQuarter
  | CustomPeriod !DateRange
  deriving stock (Eq, Ord, Show)

-- | Elapsed duration in years for DCF discounting.
newtype YearFraction = UnsafeYearFraction Double
  deriving stock (Eq, Ord, Show)

-- | Construct a finite, non-negative duration.
mkYearFraction :: Double -> Either PeriodError YearFraction
mkYearFraction duration
  | isNaN duration || isInfinite duration = Left (NonFiniteYearFraction (Text.pack (show duration)))
  | duration < 0 = Left (NegativeYearFraction (Text.pack (show duration)))
  | otherwise = Right (UnsafeYearFraction duration)

unYearFraction :: YearFraction -> Double
unYearFraction (UnsafeYearFraction duration) = duration

-- | Compute elapsed years using Actual/365, rejecting reversed dates and overflow.
diffDaysYearFraction :: Day -> Day -> Either PeriodError YearFraction
diffDaysYearFraction baseDay targetDay
  | targetDay < baseDay = Left (ReversedDateInterval baseDay targetDay)
  | otherwise = mkYearFraction (fromIntegral (diffDays targetDay baseDay) / 365.0)

-- | Indian fiscal year bounds: April 1 through March 31, inclusive.
fiscalYearBounds :: FiscalYear -> (Day, Day)
fiscalYearBounds fiscalYear =
  let year = unFiscalYear fiscalYear
   in (fromGregorian (year - 1) 4 1, fromGregorian year 3 31)

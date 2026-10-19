{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Core domain types and algebraic data types for Seby.
Implements total, law-abiding representation of financial entities,
dimensions, and data-quality states.
-}
module Seby.Domain.Types (
  -- * Identifiers
  CompanyId (..),
  SecurityId (..),
  Symbol (..),
  SourceId (..),

  -- * Provenance & Sources
  Provenance (..),

  -- * Financial Accounting Dimensions
  StatementBasis (..),
  PeriodType (..),
  ConsolidationScope (..),
  Sign (..),

  -- * Dimensioned Quantities
  Rate,
  mkRate,
  unRate,
  Multiple,
  mkMultiple,
  unMultiple,
  Days (..),
  PositiveYears,
  mkPositiveYears,
  unPositiveYears,
  yearsToDouble,

  -- * GADT-Indexed Metrics
  Metric (..),

  -- * Data Quality & Availability
  DataIssue (..),
  ObservationValue (..),
  Observed (..),
) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Time (UTCTime)
import Seby.Domain.Money (Money)

-- ----------------------------------------------------------------------------
-- Identifiers
-- ----------------------------------------------------------------------------

-- | Stable internal company identifier (independent of changing tickers).
newtype CompanyId = CompanyId {unCompanyId :: Text}
  deriving stock (Eq, Ord, Show)

-- | Listed security identifier (e.g. NSE EQ, BSE scrip).
newtype SecurityId = SecurityId {unSecurityId :: Text}
  deriving stock (Eq, Ord, Show)

-- | Ticker symbol on an exchange.
newtype Symbol = Symbol {unSymbol :: Text}
  deriving stock (Eq, Ord, Show)

-- | Sourced data provider identifier (e.g. NSE, BSE, AnnualReport).
newtype SourceId = SourceId {unSourceId :: Text}
  deriving stock (Eq, Ord, Show)

-- ----------------------------------------------------------------------------
-- Provenance
-- ----------------------------------------------------------------------------

-- | Complete cryptographic and temporal lineage of an observation.
data Provenance = Provenance
  { provenanceSource :: !SourceId
  , provenanceUri :: !(Maybe Text)
  , provenanceRetrievedAt :: !UTCTime
  , provenancePublishedAt :: !(Maybe UTCTime)
  , provenanceDigest :: !Text
  }
  deriving stock (Eq, Show)

-- ----------------------------------------------------------------------------
-- Financial Dimensions (ADTs)
-- ----------------------------------------------------------------------------

-- | Statement reporting basis under Ind AS.
data StatementBasis
  = Standalone
  | Consolidated
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Fiscal period duration type.
data PeriodType
  = Annual
  | Quarterly
  | TrailingTwelveMonths
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Consolidation scope for analysis.
data ConsolidationScope
  = GroupScope
  | SegmentScope !Text
  deriving stock (Eq, Ord, Show)

-- | Algebraic sign of a financial quantity.
data Sign
  = Positive
  | Negative
  | Zero
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- ----------------------------------------------------------------------------
-- Dimensioned Newtypes with Smart Constructors
-- ----------------------------------------------------------------------------

-- | Dimensionless rate or percentage (e.g. 0.15 for 15%).
newtype Rate = UnsafeRate Double
  deriving stock (Eq, Ord, Show)

-- | Smart constructor for Rate, rejecting non-finite IEEE 754 floats.
mkRate :: Double -> Either Text Rate
mkRate r
  | isNaN r || isInfinite r = Left "Rate must be a finite real number"
  | otherwise = Right (UnsafeRate r)

unRate :: Rate -> Double
unRate (UnsafeRate r) = r

-- | Valuation or financial multiple (e.g. 24.5x P/E).
newtype Multiple = UnsafeMultiple Double
  deriving stock (Eq, Ord, Show)

-- | Smart constructor for Multiple, rejecting non-finite IEEE 754 floats.
mkMultiple :: Double -> Either Text Multiple
mkMultiple m
  | isNaN m || isInfinite m = Left "Multiple must be a finite real number"
  | otherwise = Right (UnsafeMultiple m)

unMultiple :: Multiple -> Double
unMultiple (UnsafeMultiple m) = m

-- | Integer number of days (e.g. Days Sales Outstanding).
newtype Days = Days {unDays :: Integer}
  deriving stock (Eq, Ord, Show)

-- | Strictly positive number of compounding years for growth calculations.
newtype PositiveYears = UnsafePositiveYears Double
  deriving stock (Eq, Ord, Show)

mkPositiveYears :: Double -> Either Text PositiveYears
mkPositiveYears y
  | isNaN y || isInfinite y = Left "Years must be finite"
  | y <= 0 = Left "Compounding period must be strictly positive"
  | otherwise = Right (UnsafePositiveYears y)

unPositiveYears :: PositiveYears -> Double
unPositiveYears (UnsafePositiveYears y) = y

yearsToDouble :: PositiveYears -> Double
yearsToDouble = unPositiveYears

-- ----------------------------------------------------------------------------
-- GADT-Indexed Financial Metrics
-- ----------------------------------------------------------------------------

{- | GADT indexing every supported metric by its exact dimensional return type.
Prevents mixing Money, Rate, Multiple, and Days at compile time.
-}
data Metric a where
  -- Monetary Metrics (Money)
  Revenue :: Metric Money
  OperatingProfit :: Metric Money
  NetProfit :: Metric Money
  OperatingCashFlow :: Metric Money
  CapitalExpenditure :: Metric Money
  FreeCashFlow :: Metric Money
  -- Percentage / Rate Metrics (Rate)
  GrossMargin :: Metric Rate
  OperatingMargin :: Metric Rate
  NetMargin :: Metric Rate
  ReturnOnEquity :: Metric Rate
  ReturnOnCapital :: Metric Rate
  RevenueCAGR :: Metric Rate
  ProfitCAGR :: Metric Rate
  -- Valuation Multiples (Multiple)
  PriceToEarnings :: Metric Multiple
  PriceToBook :: Metric Multiple
  EVToEBITDA :: Metric Multiple
  DebtToEquity :: Metric Multiple
  -- Operating Cycle (Days)
  DaysSalesOut :: Metric Days
  DaysInventoryOut :: Metric Days
  CashConversionCyc :: Metric Days

-- ----------------------------------------------------------------------------
-- Data Quality and Availability
-- ----------------------------------------------------------------------------

-- | Explicit reasons why a financial fact or metric cannot be computed or trusted.
data DataIssue
  = NotDisclosed
  | NotRetrieved
  | NotApplicable
  | NotMeaningful !Text
  | InvalidInput !Text
  | NonComparable !Text
  | StaleObservation !UTCTime
  | ConflictingSources !(NonEmpty SourceId)
  deriving stock (Eq, Show)

-- | Successful observation pairing a value with its non-empty provenance history.
data ObservationValue a = ObservationValue
  { obsValue :: !a
  , obsProvenance :: !(NonEmpty Provenance)
  }
  deriving stock (Eq, Show)

{- | Typed container distinguishing available data with lineage from explicit issues.
Uses complete constructors without partial record accessors.
-}
data Observed a
  = Available !(ObservationValue a)
  | Unavailable !DataIssue
  deriving stock (Eq, Show)

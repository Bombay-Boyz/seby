{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Pure financial calculation engine for Seby.
Implements total, law-abiding financial formulas grounded in CFA Institute FRA
standards and Damodaran corporate valuation principles.
-}
module Seby.Metrics (
  MetricError (..),
  growthRate,
  freeCashFlow,
  grossMargin,
  operatingMargin,
  netMargin,
  returnOnEquity,
  returnOnCapitalEmployed,
  debtToEquity,
  interestCoverage,
  currentRatio,
  cashConversionRatio,
  daysSalesOutstanding,
  daysInventoryOutstanding,
  discountCashFlow,
  priceToEarnings,
  priceToBook,
) where

import Data.Text (Text)
import Seby.Domain.Money (
  Money,
  moneyAdd,
  moneyRatio,
  moneyScale,
  moneySign,
  moneySubtract,
 )
import Seby.Domain.Period (YearFraction, unYearFraction)
import Seby.Domain.Types (
  Days (..),
  Multiple,
  PositiveYears,
  Rate,
  mkMultiple,
  mkRate,
  unRate,
  yearsToDouble,
 )

-- | Explicit domain errors raised during financial metric computation.
data MetricError
  = NonPositiveGrowthBoundary !Text -- CFA FRA: CAGR is undefined when base or target <= 0
  | ZeroDenominator !Text
  | IncompatibleCurrencies !Text
  | InvalidMetricInput !Text
  | NonFiniteResult !Text
  deriving stock (Eq, Show)

-- | Convert exact rational ratio to Double.
ratioToDouble :: Rational -> Double
ratioToDouble = fromRational

-- ----------------------------------------------------------------------------
-- 1. Growth Metrics
-- ----------------------------------------------------------------------------

{- | Compound Annual Growth Rate (CAGR) following CFA Institute standards.
Formula: (V_n / V_0)**(1/n) - 1.
Strictly defined only when both base and ending values are strictly positive.
-}
growthRate :: PositiveYears -> Money -> Money -> Either MetricError Rate
growthRate years oldValue newValue
  | moneySign oldValue /= GT =
      Left (NonPositiveGrowthBoundary "Base period value must be strictly positive for CAGR")
  | moneySign newValue /= GT =
      Left (NonPositiveGrowthBoundary "Target period value must be strictly positive for CAGR")
  | otherwise = do
      ratio <- case moneyRatio newValue oldValue of
        Left err -> Left (IncompatibleCurrencies err)
        Right r -> Right r
      let n = yearsToDouble years
          ratioD = ratioToDouble ratio
          cagr = (ratioD ** (1.0 / n)) - 1.0
      case mkRate cagr of
        Left err -> Left (NonFiniteResult err)
        Right r -> Right r

-- ----------------------------------------------------------------------------
-- 2. Cash Flow Metrics
-- ----------------------------------------------------------------------------

-- | Free Cash Flow = Operating Cash Flow - Capital Expenditure.
freeCashFlow :: Money -> Money -> Either MetricError Money
freeCashFlow ocf capex =
  case moneySubtract ocf capex of
    Left err -> Left (IncompatibleCurrencies err)
    Right fcf -> Right fcf

-- | Cash Conversion Ratio = Operating Cash Flow / Net Income.
cashConversionRatio :: Money -> Money -> Either MetricError Multiple
cashConversionRatio ocf netIncome
  | moneySign netIncome == EQ =
      Left (ZeroDenominator "Net income cannot be zero for cash conversion ratio")
  | otherwise = do
      ratio <- case moneyRatio ocf netIncome of
        Left err -> Left (IncompatibleCurrencies err)
        Right r -> Right r
      let multD = ratioToDouble ratio
      case mkMultiple multD of
        Left err -> Left (NonFiniteResult err)
        Right m -> Right m

-- ----------------------------------------------------------------------------
-- 3. Profitability & Margin Metrics
-- ----------------------------------------------------------------------------

-- | Compute margin ratio = Numerator / Revenue.
marginRatio :: Text -> Money -> Money -> Either MetricError Rate
marginRatio metricName numerator revenue
  | moneySign revenue /= GT =
      Left (ZeroDenominator (metricName <> " cannot be calculated on zero or negative revenue"))
  | otherwise = do
      ratio <- case moneyRatio numerator revenue of
        Left err -> Left (IncompatibleCurrencies err)
        Right r -> Right r
      let rateD = ratioToDouble ratio
      case mkRate rateD of
        Left err -> Left (NonFiniteResult err)
        Right r -> Right r

-- | Gross Margin = Gross Profit / Revenue.
grossMargin :: Money -> Money -> Either MetricError Rate
grossMargin = marginRatio "Gross margin"

-- | Operating Margin = Operating Profit (EBIT) / Revenue.
operatingMargin :: Money -> Money -> Either MetricError Rate
operatingMargin = marginRatio "Operating margin"

-- | Net Profit Margin = Net Profit / Revenue.
netMargin :: Money -> Money -> Either MetricError Rate
netMargin = marginRatio "Net profit margin"

-- ----------------------------------------------------------------------------
-- 4. Returns on Capital
-- ----------------------------------------------------------------------------

-- | Return on Equity (ROE) = Net Profit / Average Shareholder Equity.
returnOnEquity :: Money -> Money -> Either MetricError Rate
returnOnEquity netIncome equity
  | moneySign equity /= GT =
      Left (ZeroDenominator "Shareholder equity must be strictly positive for ROE")
  | otherwise = do
      ratio <- case moneyRatio netIncome equity of
        Left err -> Left (IncompatibleCurrencies err)
        Right r -> Right r
      let roeD = ratioToDouble ratio
      case mkRate roeD of
        Left err -> Left (NonFiniteResult err)
        Right r -> Right r

-- | Return on Capital Employed (ROCE) = EBIT / Capital Employed.
returnOnCapitalEmployed :: Money -> Money -> Either MetricError Rate
returnOnCapitalEmployed ebit capitalEmployed
  | moneySign capitalEmployed /= GT =
      Left (ZeroDenominator "Capital employed must be strictly positive for ROCE")
  | otherwise = do
      ratio <- case moneyRatio ebit capitalEmployed of
        Left err -> Left (IncompatibleCurrencies err)
        Right r -> Right r
      let roceD = ratioToDouble ratio
      case mkRate roceD of
        Left err -> Left (NonFiniteResult err)
        Right r -> Right r

-- ----------------------------------------------------------------------------
-- 5. Balance Sheet & Liquidity Ratios
-- ----------------------------------------------------------------------------

-- | Debt-to-Equity = Total Debt / Shareholder Equity.
debtToEquity :: Money -> Money -> Either MetricError Multiple
debtToEquity totalDebt equity
  | moneySign equity /= GT =
      Left (ZeroDenominator "Shareholder equity must be strictly positive for Debt/Equity")
  | otherwise = do
      ratio <- case moneyRatio totalDebt equity of
        Left err -> Left (IncompatibleCurrencies err)
        Right r -> Right r
      let deD = ratioToDouble ratio
      case mkMultiple deD of
        Left err -> Left (NonFiniteResult err)
        Right m -> Right m

-- | Interest Coverage = EBIT / Interest Expense.
interestCoverage :: Money -> Money -> Either MetricError Multiple
interestCoverage ebit interest
  | moneySign interest /= GT =
      Left (ZeroDenominator "Interest expense must be positive for interest coverage")
  | otherwise = do
      ratio <- case moneyRatio ebit interest of
        Left err -> Left (IncompatibleCurrencies err)
        Right r -> Right r
      let icD = ratioToDouble ratio
      case mkMultiple icD of
        Left err -> Left (NonFiniteResult err)
        Right m -> Right m

-- | Current Ratio = Current Assets / Current Liabilities.
currentRatio :: Money -> Money -> Either MetricError Multiple
currentRatio currentAssets currentLiabilities
  | moneySign currentLiabilities /= GT =
      Left (ZeroDenominator "Current liabilities must be positive for current ratio")
  | otherwise = do
      ratio <- case moneyRatio currentAssets currentLiabilities of
        Left err -> Left (IncompatibleCurrencies err)
        Right r -> Right r
      let crD = ratioToDouble ratio
      case mkMultiple crD of
        Left err -> Left (NonFiniteResult err)
        Right m -> Right m

-- ----------------------------------------------------------------------------
-- 6. Working Capital & Operating Efficiency
-- ----------------------------------------------------------------------------

-- | Days Sales Outstanding uses average opening/closing receivables.
daysSalesOutstanding :: Money -> Money -> Money -> Either MetricError Days
daysSalesOutstanding = averageWorkingCapitalDays "Receivables" "Revenue" "DSO"

-- | Days Inventory Outstanding uses average opening/closing inventory.
daysInventoryOutstanding :: Money -> Money -> Money -> Either MetricError Days
daysInventoryOutstanding = averageWorkingCapitalDays "Inventory" "Cost of goods sold" "DIO"

averageWorkingCapitalDays ::
  Text -> Text -> Text -> Money -> Money -> Money -> Either MetricError Days
averageWorkingCapitalDays balanceName flowName metricName openingBalance closingBalance flow
  | moneySign flow /= GT =
      Left (ZeroDenominator (flowName <> " must be positive for " <> metricName <> "."))
  | otherwise = do
      balanceTotal <- case moneyAdd openingBalance closingBalance of
        Left err -> Left (IncompatibleCurrencies err)
        Right total -> Right total
      if moneySign balanceTotal == LT
        then
          Left (InvalidMetricInput (balanceName <> " average cannot be negative for " <> metricName <> "."))
        else do
          ratio <- case moneyRatio balanceTotal flow of
            Left err -> Left (IncompatibleCurrencies err)
            Right value -> Right value
          Right (Days (round (ratio * 365 / 2)))

-- ----------------------------------------------------------------------------
-- 7. Valuation & Discounting
-- ----------------------------------------------------------------------------

{- | Discount a single cash flow: PV = CF / (1 + r)**t.
Following Damodaran, discount factor uses elapsed year fraction t (\Delta t = t_i - t_0).
-}
discountCashFlow :: Rate -> YearFraction -> Money -> Either MetricError Money
discountCashFlow discountRate yearFraction cashFlow = do
  let r = unRate discountRate
      t = unYearFraction yearFraction
  if r <= -1.0
    then Left (NonFiniteResult "Discount rate must be strictly greater than -100%")
    else do
      let discountFactor = (1.0 + r) ** t
      if isNaN discountFactor || isInfinite discountFactor || discountFactor == 0
        then Left (NonFiniteResult "Invalid discount factor calculation")
        else
          let pvRatio = toRational (1.0 / discountFactor)
           in Right (moneyScale pvRatio cashFlow)

-- | Price to Earnings (P/E) Multiple = Market Capitalisation / Net Profit.
priceToEarnings :: Money -> Money -> Either MetricError Multiple
priceToEarnings marketCap netProfit
  | moneySign netProfit /= GT =
      Left
        (NonPositiveGrowthBoundary "P/E is not economically meaningful for zero or loss-making net profit")
  | otherwise = do
      ratio <- case moneyRatio marketCap netProfit of
        Left err -> Left (IncompatibleCurrencies err)
        Right r -> Right r
      let peD = ratioToDouble ratio
      case mkMultiple peD of
        Left err -> Left (NonFiniteResult err)
        Right m -> Right m

-- | Price to Book (P/B) Multiple = Market Capitalisation / Total Equity.
priceToBook :: Money -> Money -> Either MetricError Multiple
priceToBook marketCap totalEquity
  | moneySign totalEquity /= GT =
      Left (ZeroDenominator "Total equity must be positive for P/B multiple")
  | otherwise = do
      ratio <- case moneyRatio marketCap totalEquity of
        Left err -> Left (IncompatibleCurrencies err)
        Right r -> Right r
      let pbD = ratioToDouble ratio
      case mkMultiple pbD of
        Left err -> Left (NonFiniteResult err)
        Right m -> Right m

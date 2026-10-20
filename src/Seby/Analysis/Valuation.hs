{-# LANGUAGE DerivingStrategies #-}

{- | Explicit discounted-cash-flow valuation and sensitivity tools.

The model produces enterprise value from supplied free cash-flow forecasts. It
does not infer a cost of capital, forecast growth, net debt, or equity value.
Those inputs and any enterprise-to-equity bridge remain the analyst's job.
-}
module Seby.Analysis.Valuation (
  CashFlowTiming (..),
  ProjectedCashFlow,
  mkProjectedCashFlow,
  projectedCashFlowTime,
  projectedCashFlowAmount,
  DcfAssumptions,
  mkDcfAssumptions,
  assumptionsDiscountRate,
  assumptionsTerminalGrowthRate,
  assumptionsCashFlowTiming,
  assumptionsForecasts,
  ValuationError (..),
  DcfValuation (..),
  valueDcf,
  ScenarioName (..),
  ScenarioValuation (..),
  valueBearBaseBull,
  SensitivityPoint (..),
  sensitivityGrid,
  impliedTerminalGrowth,
) where

import Control.Monad (when)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Seby.Domain.Money (
  Money,
  moneyAdd,
  moneyCurrency,
  moneyRatio,
  moneyScale,
  moneySign,
  moneySubtract,
 )
import Seby.Domain.Period (YearFraction, mkYearFraction, unYearFraction)
import Seby.Domain.Types (Rate, mkRate, unRate)
import Seby.Metrics (MetricError, discountCashFlow)

data CashFlowTiming = YearEnd | MidYear
  deriving stock (Eq, Ord, Show)

data ProjectedCashFlow = ProjectedCashFlow !YearFraction !Money
  deriving stock (Eq, Show)

mkProjectedCashFlow :: YearFraction -> Money -> ProjectedCashFlow
mkProjectedCashFlow = ProjectedCashFlow

projectedCashFlowTime :: ProjectedCashFlow -> YearFraction
projectedCashFlowTime (ProjectedCashFlow time _) = time

projectedCashFlowAmount :: ProjectedCashFlow -> Money
projectedCashFlowAmount (ProjectedCashFlow _ amount) = amount

data DcfAssumptions = DcfAssumptions
  { assumptionsDiscountRate :: !Rate
  , assumptionsTerminalGrowthRate :: !Rate
  , assumptionsCashFlowTiming :: !CashFlowTiming
  , assumptionsForecasts :: !(NonEmpty ProjectedCashFlow)
  }
  deriving stock (Eq, Show)

data ValuationError
  = InvalidDiscountRate !Double
  | InvalidTerminalGrowthRate !Double
  | TerminalGrowthMustBeBelowDiscountRate !Double !Double
  | NonIncreasingForecastPeriods !Double !Double
  | MidYearCashFlowBeforeMidYear !Double
  | InconsistentForecastCurrencies
  | NonPositiveTerminalCashFlow
  | ValuationCurrencyMismatch
  | NoPositiveImpliedTerminalValue
  | ImpliedGrowthOutsideModelDomain !Double
  | NonFiniteValuation !Text
  | ValuationMetricFailure !MetricError
  deriving stock (Eq, Show)

{- | Validate discount/growth domains, strictly increasing forecast times,
consistent currencies, and a positive terminal cash flow. Forecasts may contain
negative values; those are retained as supplied.
-}
mkDcfAssumptions ::
  Rate -> Rate -> CashFlowTiming -> NonEmpty ProjectedCashFlow -> Either ValuationError DcfAssumptions
mkDcfAssumptions discountRate terminalGrowthRate timing forecasts = do
  let discount = unRate discountRate
      growth = unRate terminalGrowthRate
  when (discount <= -1) (Left (InvalidDiscountRate discount))
  when (growth <= -1) (Left (InvalidTerminalGrowthRate growth))
  when (growth >= discount) (Left (TerminalGrowthMustBeBelowDiscountRate discount growth))
  validateForecasts timing forecasts
  pure (DcfAssumptions discountRate terminalGrowthRate timing forecasts)

validateForecasts :: CashFlowTiming -> NonEmpty ProjectedCashFlow -> Either ValuationError ()
validateForecasts timing forecasts = do
  let entries = NonEmpty.toList forecasts
      expectedCurrency = case forecasts of
        first :| _ -> moneyCurrency (projectedCashFlowAmount first)
  if all ((== expectedCurrency) . moneyCurrency . projectedCashFlowAmount) entries
    then pure ()
    else Left InconsistentForecastCurrencies
  validateTimes timing entries
  if moneySign (projectedCashFlowAmount (lastValue forecasts)) == GT
    then pure ()
    else Left NonPositiveTerminalCashFlow

validateTimes :: CashFlowTiming -> [ProjectedCashFlow] -> Either ValuationError ()
validateTimes _ [] = Right ()
validateTimes timing (first : rest) = do
  let firstTime = unYearFraction (projectedCashFlowTime first)
      adjustedTime = if timing == MidYear then firstTime - 0.5 else firstTime
  if adjustedTime < 0
    then Left (MidYearCashFlowBeforeMidYear firstTime)
    else validatePairs first rest
  where
    validatePairs _ [] = Right ()
    validatePairs previous (current : remaining) =
      let previousTime = unYearFraction (projectedCashFlowTime previous)
          currentTime = unYearFraction (projectedCashFlowTime current)
          adjustedTime = if timing == MidYear then currentTime - 0.5 else currentTime
       in if currentTime <= previousTime
            then Left (NonIncreasingForecastPeriods previousTime currentTime)
            else
              if adjustedTime < 0
                then Left (MidYearCashFlowBeforeMidYear currentTime)
                else validatePairs current remaining

data DcfValuation = DcfValuation
  { dcfForecastPresentValue :: !Money
  , dcfTerminalValue :: !Money
  , dcfTerminalPresentValue :: !Money
  , dcfEnterpriseValue :: !Money
  }
  deriving stock (Eq, Show)

valueDcf :: DcfAssumptions -> Either ValuationError DcfValuation
valueDcf assumptions = do
  forecastPresentValue <-
    forecastPresentValueAt
      (assumptionsDiscountRate assumptions)
      (assumptionsCashFlowTiming assumptions)
      (assumptionsForecasts assumptions)
  let terminalForecast = lastValue (assumptionsForecasts assumptions)
  terminalValue <-
    calculateTerminalValue
      (assumptionsDiscountRate assumptions)
      (assumptionsTerminalGrowthRate assumptions)
      (projectedCashFlowAmount terminalForecast)
  terminalPresentValue <-
    discountAt
      (assumptionsDiscountRate assumptions)
      (projectedCashFlowTime terminalForecast)
      terminalValue
  enterpriseValue <- addMoney forecastPresentValue terminalPresentValue
  pure
    DcfValuation
      { dcfForecastPresentValue = forecastPresentValue
      , dcfTerminalValue = terminalValue
      , dcfTerminalPresentValue = terminalPresentValue
      , dcfEnterpriseValue = enterpriseValue
      }

forecastPresentValueAt ::
  Rate -> CashFlowTiming -> NonEmpty ProjectedCashFlow -> Either ValuationError Money
forecastPresentValueAt discountRate timing forecasts = do
  presentValues <- traverse discountForecast (NonEmpty.toList forecasts)
  sumMoney presentValues
  where
    discountForecast (ProjectedCashFlow time cashFlow) =
      let timeValue = unYearFraction time - if timing == MidYear then 0.5 else 0
       in do
            yearFraction <- case mkYearFraction timeValue of
              Left err -> Left (NonFiniteValuation (fromStringShow err))
              Right value -> Right value
            discountAt discountRate yearFraction cashFlow

calculateTerminalValue :: Rate -> Rate -> Money -> Either ValuationError Money
calculateTerminalValue discountRate growthRate cashFlow = do
  let discount = unRate discountRate
      growth = unRate growthRate
      terminalMultiplier = (1 + growth) / (discount - growth)
  if isNaN terminalMultiplier || isInfinite terminalMultiplier || terminalMultiplier <= 0
    then Left (NonFiniteValuation "Gordon growth terminal multiplier is invalid")
    else pure (moneyScale (toRational terminalMultiplier) cashFlow)

discountAt :: Rate -> YearFraction -> Money -> Either ValuationError Money
discountAt discountRate time cashFlow =
  case discountCashFlow discountRate time cashFlow of
    Left err -> Left (ValuationMetricFailure err)
    Right value -> Right value

addMoney :: Money -> Money -> Either ValuationError Money
addMoney left right = case moneyAdd left right of
  Left _ -> Left ValuationCurrencyMismatch
  Right value -> Right value

subtractMoney :: Money -> Money -> Either ValuationError Money
subtractMoney left right = case moneySubtract left right of
  Left _ -> Left ValuationCurrencyMismatch
  Right value -> Right value

sumMoney :: [Money] -> Either ValuationError Money
sumMoney [] = Left (NonFiniteValuation "Cash-flow present value list was empty")
sumMoney (first : rest) = foldl addStep (Right first) rest
  where
    addStep accumulated value = accumulated >>= (`addMoney` value)

data ScenarioName = Bear | Base | Bull
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data ScenarioValuation = ScenarioValuation
  { scenarioName :: !ScenarioName
  , scenarioResult :: !DcfValuation
  }
  deriving stock (Eq, Show)

valueBearBaseBull ::
  DcfAssumptions ->
  DcfAssumptions ->
  DcfAssumptions ->
  Either ValuationError (NonEmpty ScenarioValuation)
valueBearBaseBull bear base bull = do
  bearResult <- ScenarioValuation Bear <$> valueDcf bear
  baseResult <- ScenarioValuation Base <$> valueDcf base
  bullResult <- ScenarioValuation Bull <$> valueDcf bull
  pure (bearResult :| [baseResult, bullResult])

data SensitivityPoint = SensitivityPoint
  { sensitivityDiscountRate :: !Rate
  , sensitivityTerminalGrowthRate :: !Rate
  , sensitivityEnterpriseValue :: !Money
  }
  deriving stock (Eq, Show)

{- | Evaluate a full discount-rate by terminal-growth grid using the same cash
flows and timing convention. Each grid cell is independently validated.
-}
sensitivityGrid ::
  DcfAssumptions -> NonEmpty Rate -> NonEmpty Rate -> Either ValuationError [SensitivityPoint]
sensitivityGrid base discountRates growthRates =
  traverse
    evaluate
    [ (discount, growth)
    | discount <- NonEmpty.toList discountRates
    , growth <- NonEmpty.toList growthRates
    ]
  where
    evaluate (discount, growth) = do
      assumptions <-
        mkDcfAssumptions discount growth (assumptionsCashFlowTiming base) (assumptionsForecasts base)
      valuation <- valueDcf assumptions
      pure (SensitivityPoint discount growth (dcfEnterpriseValue valuation))

{- | Solve for the Gordon-growth terminal rate implied by a target enterprise
value while holding forecast cash flows and the discount rate fixed.
-}
impliedTerminalGrowth :: DcfAssumptions -> Money -> Either ValuationError Rate
impliedTerminalGrowth assumptions targetEnterpriseValue = do
  let forecasts = assumptionsForecasts assumptions
      discountRate = assumptionsDiscountRate assumptions
      timing = assumptionsCashFlowTiming assumptions
      firstForecast = case forecasts of
        first :| _ -> first
      terminalForecast = lastValue forecasts
  when
    (moneyCurrency targetEnterpriseValue /= moneyCurrency (projectedCashFlowAmount firstForecast))
    (Left ValuationCurrencyMismatch)
  forecastPV <- forecastPresentValueAt discountRate timing forecasts
  terminalPresentValue <- subtractMoney targetEnterpriseValue forecastPV
  when (moneySign terminalPresentValue /= GT) (Left NoPositiveImpliedTerminalValue)
  let elapsed = unYearFraction (projectedCashFlowTime terminalForecast)
      discount = unRate discountRate
      discountFactor = (1 + discount) ** elapsed
  when
    (isNaN discountFactor || isInfinite discountFactor || discountFactor <= 0)
    (Left (NonFiniteValuation "Invalid terminal-value discount factor"))
  let undiscountedTerminal = moneyScale (toRational discountFactor) terminalPresentValue
  let terminalCashFlow = projectedCashFlowAmount terminalForecast
  ratio <- case moneyRatio undiscountedTerminal terminalCashFlow of
    Left _ -> Left ValuationCurrencyMismatch
    Right value -> Right (fromRational value :: Double)
  let impliedGrowth = (ratio * discount - 1) / (ratio + 1)
  if isNaN ratio || isInfinite ratio || ratio <= 0 || isNaN impliedGrowth || isInfinite impliedGrowth
    then Left (NonFiniteValuation "Implied terminal growth is not finite")
    else
      if impliedGrowth <= -1 || impliedGrowth >= discount
        then Left (ImpliedGrowthOutsideModelDomain impliedGrowth)
        else case mkRate impliedGrowth of
          Left _ -> Left (NonFiniteValuation "Implied terminal growth is not finite")
          Right rate -> Right rate

fromStringShow :: (Show a) => a -> Text
fromStringShow = Text.pack . show

lastValue :: NonEmpty a -> a
lastValue (first :| rest) = foldl (\_ next -> next) first rest

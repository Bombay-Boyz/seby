{-# LANGUAGE OverloadedStrings #-}

module Seby.ValuationSpec (runTests) where

import Data.List.NonEmpty (NonEmpty (..))
import Seby.Analysis.Valuation
import Seby.Domain.Money (Currency (..), Money, fromRupees, mkMoney, moneyAdd, toRupeesDouble)
import Seby.Domain.Period (YearFraction, mkYearFraction)
import Seby.Domain.Types (Rate, mkRate, unRate)

runTests :: IO ()
runTests = do
  baseFlows <- forecast [(1, fromRupees 100), (2, fromRupees 110)]
  discount10 <- rate 0.10
  discount8 <- rate 0.08
  discount12 <- rate 0.12
  growth2 <- rate 0.02
  growth1 <- rate 0.01
  growth3 <- rate 0.03
  base <- assumptions discount10 growth2 YearEnd baseFlows
  baseValue <- expectRight "base DCF" (valueDcf base)
  assert
    "forecast and terminal components sum to enterprise value"
    ( addForTest (dcfForecastPresentValue baseValue) (dcfTerminalPresentValue baseValue)
        == Just (dcfEnterpriseValue baseValue)
    )
  assert "DCF returns a positive enterprise value" (toRupeesDouble (dcfEnterpriseValue baseValue) > 0)

  bear <- assumptions discount12 growth1 YearEnd baseFlows
  bull <- assumptions discount8 growth3 YearEnd baseFlows
  scenarios <- expectRight "bear/base/bull scenarios" (valueBearBaseBull bear base bull)
  assert
    "scenario names preserve requested order"
    (map scenarioName (toList scenarios) == [Bear, Base, Bull])
  scenarioValues <- traverse (expectRight "scenario value" . valueDcf) [bear, base, bull]
  case scenarioValues of
    [bearValue, baseScenarioValue, bullValue] -> do
      assert "bear case below base" (dcfEnterpriseValue bearValue < dcfEnterpriseValue baseScenarioValue)
      assert "base case below bull" (dcfEnterpriseValue baseScenarioValue < dcfEnterpriseValue bullValue)
    _ -> fail "scenario fixture shape changed"

  grid <-
    expectRight
      "two by two sensitivity grid"
      (sensitivityGrid base (discount8 :| [discount12]) (growth1 :| [growth3]))
  assert "sensitivity grid has four cells" (length grid == 4)
  assert
    "higher discount rate lowers value at fixed growth"
    ( case (cellValue discount8 growth1 grid, cellValue discount12 growth1 grid) of
        (Just lowerRateValue, Just higherRateValue) -> toRupeesDouble lowerRateValue > toRupeesDouble higherRateValue
        _ -> False
    )
  assert
    "higher terminal growth raises value at fixed discount"
    ( case (cellValue discount8 growth3 grid, cellValue discount8 growth1 grid) of
        (Just higherGrowthValue, Just lowerGrowthValue) -> toRupeesDouble higherGrowthValue > toRupeesDouble lowerGrowthValue
        _ -> False
    )

  implied <-
    expectRight
      "reverse DCF recovers base terminal growth"
      (impliedTerminalGrowth base (dcfEnterpriseValue baseValue))
  assert "reverse DCF growth matches input within rounding" (abs (unRate implied - 0.02) < 0.00001)
  assert "reverse DCF rejects target below forecast value" $
    isNoPositiveTerminalValue (impliedTerminalGrowth base (fromRupees 1))

  yearOne <- yearFraction 1
  yearQuarter <- yearFraction 0.25
  yearTwo <- yearFraction 2
  let midYearFlows = mkProjectedCashFlow yearOne (fromRupees 110) :| []
  midYear <- assumptions discount10 growth2 MidYear midYearFlows
  yearEnd <- assumptions discount10 growth2 YearEnd midYearFlows
  midYearValue <- expectRight "mid-year DCF" (valueDcf midYear)
  yearEndValue <- expectRight "year-end DCF" (valueDcf yearEnd)
  assert "mid-year convention discounts forecast cash flow for less time" $
    dcfForecastPresentValue midYearValue > dcfForecastPresentValue yearEndValue

  assert "terminal growth equal to discount rate rejected" $
    isGrowthNotBelowDiscount (mkDcfAssumptions discount10 discount10 YearEnd baseFlows)
  assert "mid-year cash flow before midpoint rejected" $
    isMidYearTimeError
      (mkDcfAssumptions discount10 growth2 MidYear (mkProjectedCashFlow yearQuarter (fromRupees 110) :| []))
  assert "non-positive terminal cash flow rejected" $
    isNonPositiveTerminal
      (mkDcfAssumptions discount10 growth2 YearEnd (mkProjectedCashFlow yearOne (fromRupees 0) :| []))
  assert "mixed forecast currencies rejected" $
    isMixedCurrency
      ( mkDcfAssumptions
          discount10
          growth2
          YearEnd
          (mkProjectedCashFlow yearOne (fromRupees 10) :| [mkProjectedCashFlow yearTwo (mkMoney USD 1000)])
      )
  putStrLn
    "Valuation tests passed (scenarios, sensitivities, reverse DCF, timing, and invalid-input boundaries)."

forecast :: [(Double, Money)] -> IO (NonEmpty ProjectedCashFlow)
forecast [] = fail "forecast fixture must contain at least one cash flow"
forecast ((firstTime, firstAmount) : remaining) = do
  first <- mkProjectedCashFlow <$> yearFraction firstTime <*> pure firstAmount
  rest <-
    traverse (\(time, amount) -> mkProjectedCashFlow <$> yearFraction time <*> pure amount) remaining
  pure (first :| rest)

yearFraction :: Double -> IO YearFraction
yearFraction duration = expectRight "year fraction" (mkYearFraction duration)

rate :: Double -> IO Rate
rate value = expectRight "rate" (mkRate value)

assumptions :: Rate -> Rate -> CashFlowTiming -> NonEmpty ProjectedCashFlow -> IO DcfAssumptions
assumptions discount growth timing cashFlows =
  expectRight "DCF assumptions" (mkDcfAssumptions discount growth timing cashFlows)

toList :: NonEmpty a -> [a]
toList (first :| rest) = first : rest

addForTest :: Money -> Money -> Maybe Money
addForTest left right = case moneyAdd left right of
  Right value -> Just value
  Left _ -> Nothing

cellValue :: Rate -> Rate -> [SensitivityPoint] -> Maybe Money
cellValue discount growth points = case [ sensitivityEnterpriseValue point
                                        | point <- points
                                        , sensitivityDiscountRate point == discount
                                        , sensitivityTerminalGrowthRate point == growth
                                        ] of
  [value] -> Just value
  _ -> Nothing

expectRight :: (Show e) => String -> Either e a -> IO a
expectRight label result = case result of
  Right value -> pure value
  Left err -> fail (label <> ": " <> show err)

assert :: String -> Bool -> IO ()
assert label condition = if condition then pure () else fail ("failed: " <> label)

isNoPositiveTerminalValue :: Either ValuationError Rate -> Bool
isNoPositiveTerminalValue (Left NoPositiveImpliedTerminalValue) = True
isNoPositiveTerminalValue _ = False

isGrowthNotBelowDiscount :: Either ValuationError DcfAssumptions -> Bool
isGrowthNotBelowDiscount (Left (TerminalGrowthMustBeBelowDiscountRate _ _)) = True
isGrowthNotBelowDiscount _ = False

isMidYearTimeError :: Either ValuationError DcfAssumptions -> Bool
isMidYearTimeError (Left (MidYearCashFlowBeforeMidYear _)) = True
isMidYearTimeError _ = False

isNonPositiveTerminal :: Either ValuationError DcfAssumptions -> Bool
isNonPositiveTerminal (Left NonPositiveTerminalCashFlow) = True
isNonPositiveTerminal _ = False

isMixedCurrency :: Either ValuationError DcfAssumptions -> Bool
isMixedCurrency (Left InconsistentForecastCurrencies) = True
isMixedCurrency _ = False

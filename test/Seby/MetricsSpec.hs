{-# LANGUAGE OverloadedStrings #-}

module Seby.MetricsSpec (
  runTests,
) where

import Seby.Domain.Money
import Seby.Domain.Period
import Seby.Domain.Types
import Seby.Metrics
import System.Exit (exitFailure)
import Test.QuickCheck (
  Testable,
  maxSuccess,
  quickCheckWithResult,
  stdArgs,
 )
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Test (isSuccess)

assertEq :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEq label expected actual =
  if expected == actual
    then putStrLn $ "  [PASS] " ++ label
    else do
      putStrLn $
        "  [FAIL] " ++ label ++ "\n    Expected: " ++ show expected ++ "\n    Actual:   " ++ show actual
      exitFailure

assertTrue :: String -> Bool -> IO ()
assertTrue label condition =
  if condition
    then putStrLn $ "  [PASS] " ++ label
    else do
      putStrLn $ "  [FAIL] " ++ label
      exitFailure

expectRight :: (Show e) => String -> Either e a -> IO a
expectRight label result = case result of
  Right value -> pure value
  Left err -> do
    putStrLn $ "  [FAIL] " ++ label ++ "\n    Unexpected error: " ++ show err
    exitFailure

runTests :: IO ()
runTests = do
  putStrLn "Running Seby.Metrics tests..."

  let crore = fromCrores
      money = fromRupees
  assertEq "free cash flow subtracts capex" (Right (crore 300)) (freeCashFlow (crore 500) (crore 200))
  assertEq
    "free cash flow allows negative result"
    (Right (crore (-100)))
    (freeCashFlow (crore 100) (crore 200))
  assertTrue
    "free cash flow rejects currency mismatch"
    (isLeft (freeCashFlow (mkMoney USD 1) (mkMoney INR 1)))

  yearsOne <- expectRight "construct one-year growth period" (mkPositiveYears 1)
  yearsTwo <- expectRight "construct two-year growth period" (mkPositiveYears 2)
  cagrDouble <- expectRight "one-year CAGR doubles" (growthRate yearsOne (money 100) (money 200))
  assertEq "CAGR is 100 percent" 1.0 (unRate cagrDouble)
  cagrRoot <-
    expectRight
      "two-year CAGR computes geometric annual growth"
      (growthRate yearsTwo (money 100) (money 400))
  assertEq "two-year 4x CAGR is 100 percent" 1.0 (unRate cagrRoot)
  assertTrue
    "CAGR rejects zero base"
    (isNonPositiveGrowthError (growthRate yearsOne zeroMoney (money 1)))
  assertTrue
    "CAGR rejects negative base"
    (isNonPositiveGrowthError (growthRate yearsOne (money (-1)) (money 1)))
  assertTrue
    "CAGR rejects zero target"
    (isNonPositiveGrowthError (growthRate yearsOne (money 1) zeroMoney))
  assertTrue
    "CAGR rejects negative target"
    (isNonPositiveGrowthError (growthRate yearsOne (money 1) (money (-1))))
  assertTrue
    "CAGR rejects incompatible currencies"
    (isLeft (growthRate yearsOne (mkMoney USD 1) (mkMoney INR 2)))
  assertTrue
    "CAGR rejects overflow to non-finite rate"
    (isNonFiniteError (growthRate yearsOne (fromPaise 1) (fromPaise (10 ^ (1000 :: Integer)))))

  let revenue = crore 1000
      grossProfit = crore 400
      operatingProfit = crore 250
      netProfit = crore 150
  gross <- expectRight "gross margin calculation" (grossMargin grossProfit revenue)
  assertEq "gross margin is 40 percent" 0.4 (unRate gross)
  operating <- expectRight "operating margin calculation" (operatingMargin operatingProfit revenue)
  assertEq "operating margin is 25 percent" 0.25 (unRate operating)
  net <- expectRight "net margin calculation" (netMargin netProfit revenue)
  assertEq "net margin is 15 percent" 0.15 (unRate net)
  assertTrue "margin rejects zero revenue" (isLeft (grossMargin grossProfit zeroMoney))
  assertTrue "margin rejects negative revenue" (isLeft (grossMargin grossProfit (crore (-1))))
  assertTrue "margin rejects currency mismatch" (isLeft (netMargin (mkMoney USD 1) revenue))
  negativeMargin <- expectRight "margin permits losses" (netMargin (crore (-50)) revenue)
  assertEq "loss margin remains negative" (-0.05) (unRate negativeMargin)

  roe <- expectRight "ROE calculation" (returnOnEquity netProfit (crore 1000))
  assertEq "ROE is 15 percent" 0.15 (unRate roe)
  assertTrue "ROE rejects zero equity" (isLeft (returnOnEquity netProfit zeroMoney))
  assertTrue "ROE rejects negative equity" (isLeft (returnOnEquity netProfit (crore (-1))))
  roce <- expectRight "ROCE calculation" (returnOnCapitalEmployed operatingProfit (crore 500))
  assertEq "ROCE is 50 percent" 0.5 (unRate roce)
  assertTrue "ROCE rejects zero capital" (isLeft (returnOnCapitalEmployed operatingProfit zeroMoney))

  debtEquity <- expectRight "debt to equity calculation" (debtToEquity (crore 500) (crore 1000))
  assertEq "debt to equity is 0.5x" 0.5 (unMultiple debtEquity)
  assertTrue "debt to equity rejects zero equity" (isLeft (debtToEquity (crore 1) zeroMoney))
  interest <- expectRight "interest coverage calculation" (interestCoverage (crore 500) (crore 50))
  assertEq "interest coverage is 10x" 10.0 (unMultiple interest)
  assertTrue "interest coverage rejects zero expense" (isLeft (interestCoverage (crore 1) zeroMoney))
  assertTrue
    "interest coverage rejects negative expense"
    (isLeft (interestCoverage (crore 1) (crore (-1))))
  current <- expectRight "current ratio calculation" (currentRatio (crore 200) (crore 100))
  assertEq "current ratio is 2x" 2.0 (unMultiple current)
  assertTrue "current ratio rejects zero liabilities" (isLeft (currentRatio (crore 1) zeroMoney))
  assertTrue
    "current ratio rejects negative liabilities"
    (isLeft (currentRatio (crore 1) (crore (-1))))
  cashConversion <-
    expectRight "cash conversion calculation" (cashConversionRatio (crore 150) netProfit)
  assertEq "cash conversion is 1x" 1.0 (unMultiple cashConversion)
  assertTrue
    "cash conversion rejects zero net income"
    (isLeft (cashConversionRatio (crore 1) zeroMoney))
  cashConversionLoss <-
    expectRight
      "cash conversion permits negative net income"
      (cashConversionRatio (crore 100) (crore (-50)))
  assertEq "negative income produces signed cash conversion" (-2.0) (unMultiple cashConversionLoss)

  dso <-
    expectRight
      "DSO uses average opening and closing receivables"
      (daysSalesOutstanding (crore 50) (crore 150) (crore 1000))
  assertEq "DSO is 36.5 days rounded to even" (Days 36) dso
  dio <-
    expectRight
      "DIO uses average opening and closing inventory"
      (daysInventoryOutstanding (crore 50) (crore 150) (crore 1000))
  assertEq "DIO is 36.5 days rounded to even" (Days 36) dio
  dsoTie <-
    expectRight
      "DSO handles exact half-day ties"
      (daysSalesOutstanding (fromPaise 9) (fromPaise 9) (fromPaise 730))
  assertEq "half-day tie follows ties-to-even" (Days 4) dsoTie
  assertTrue
    "DSO rejects zero revenue"
    (isLeft (daysSalesOutstanding (crore 1) (crore 1) zeroMoney))
  assertTrue
    "DSO rejects negative revenue"
    (isLeft (daysSalesOutstanding (crore 1) (crore 1) (crore (-1))))
  assertTrue
    "DIO rejects zero cost of goods sold"
    (isLeft (daysInventoryOutstanding (crore 1) (crore 1) zeroMoney))
  assertTrue
    "DIO rejects currency mismatch"
    (isLeft (daysInventoryOutstanding (mkMoney USD 1) (mkMoney INR 1) (mkMoney INR 100)))
  assertTrue
    "DIO rejects a negative average inventory balance"
    (isLeft (daysInventoryOutstanding (crore (-2)) (crore 1) (crore 10)))
  largeDays <-
    expectRight
      "DSO supports very large exact ratios"
      ( daysSalesOutstanding
          (fromPaise (10 ^ (1000 :: Integer)))
          (fromPaise (10 ^ (1000 :: Integer)))
          (fromPaise 1)
      )
  assertTrue "large exact DSO remains an integer without floating overflow" (unDays largeDays > 0)

  rate10 <- expectRight "construct ten percent discount rate" (mkRate 0.1)
  yearOne <- expectRight "construct one-year discount duration" (mkYearFraction 1.0)
  presentValue <-
    expectRight "discount a one-year cash flow" (discountCashFlow rate10 yearOne (money 110))
  assertEq "PV of 110 at 10 percent is 100" (money 100) presentValue
  rateMinusOne <- expectRight "construct minus 100 percent rate" (mkRate (-1.0))
  assertTrue
    "discounting rejects rate at minus 100 percent"
    (isNonFiniteError (discountCashFlow rateMinusOne yearOne (money 100)))
  rateBelowMinusOne <- expectRight "construct rate below minus 100 percent" (mkRate (-1.1))
  assertTrue
    "discounting rejects rate below minus 100 percent"
    (isNonFiniteError (discountCashFlow rateBelowMinusOne yearOne (money 100)))
  yearZero <- expectRight "construct zero duration" (mkYearFraction 0)
  undiscounted <-
    expectRight
      "zero-duration cash flow remains unchanged"
      (discountCashFlow rate10 yearZero (money 123))
  assertEq "zero duration has no discount" (money 123) undiscounted
  hugeDuration <- expectRight "construct large duration" (mkYearFraction 1e308)
  assertTrue
    "discounting rejects underflowed discount factor"
    (isNonFiniteError (discountCashFlow rate10 hugeDuration (money 1)))

  pe <- expectRight "P/E calculation" (priceToEarnings (crore 3000) (crore 100))
  assertEq "P/E is 30x" 30.0 (unMultiple pe)
  assertTrue
    "P/E rejects zero earnings"
    (isNonPositiveGrowthError (priceToEarnings (crore 1) zeroMoney))
  assertTrue "P/E rejects losses" (isNonPositiveGrowthError (priceToEarnings (crore 1) (crore (-1))))
  pb <- expectRight "P/B calculation" (priceToBook (crore 2000) (crore 1000))
  assertEq "P/B is 2x" 2.0 (unMultiple pb)
  assertTrue "P/B rejects zero equity" (isLeft (priceToBook (crore 1) zeroMoney))
  assertTrue "P/B rejects currency mismatch" (isLeft (priceToBook (mkMoney USD 1) (mkMoney INR 1)))

  assertTrue
    "extreme finite Money ratios reject non-finite rate output"
    (isNonFiniteError (grossMargin (fromPaise (10 ^ (1000 :: Integer))) (fromPaise 1)))
  runProperty "equal positive values always produce zero CAGR" prop_equalValuesHaveZeroGrowth
  putStrLn "Seby.Metrics tests passed successfully."

runProperty :: (Testable prop) => String -> prop -> IO ()
runProperty label property = do
  result <- quickCheckWithResult stdArgs{maxSuccess = 500} property
  assertTrue label (isSuccess result)

prop_equalValuesHaveZeroGrowth :: QC.Positive Integer -> QC.Positive Int -> Bool
prop_equalValuesHaveZeroGrowth (QC.Positive amount) (QC.Positive years) =
  case mkPositiveYears (fromIntegral years) of
    Left _ -> False
    Right duration -> case growthRate duration (fromRupees amount) (fromRupees amount) of
      Left _ -> False
      Right rate -> unRate rate == 0

isLeft :: Either e a -> Bool
isLeft result = case result of
  Left _ -> True
  Right _ -> False

isNonPositiveGrowthError :: Either MetricError a -> Bool
isNonPositiveGrowthError result = case result of
  Left (NonPositiveGrowthBoundary _) -> True
  _ -> False

isNonFiniteError :: Either MetricError a -> Bool
isNonFiniteError result = case result of
  Left (NonFiniteResult _) -> True
  _ -> False

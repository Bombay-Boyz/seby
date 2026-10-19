module Seby.DomainSpec (
  runTests,
) where

import Data.Ratio ((%))
import Data.Time.Calendar (Day, fromGregorian)
import Seby.Domain.Money
import Seby.Domain.Period
import Seby.Domain.Types
import System.Exit (exitFailure)
import Test.QuickCheck (
  NonZero (..),
  Testable,
  maxSuccess,
  quickCheckWithResult,
  stdArgs,
 )
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

runTests :: IO ()
runTests = do
  putStrLn "Running Seby.Domain tests..."

  let rupees = fromRupees 500
      paiseCases =
        [ ("zero paise", 0, fromPaise 0)
        , ("one paise", 1, fromPaise 1)
        , ("large paise", 5000000000000000000, fromPaise 5000000000000000000)
        ]
  mapM_ (\(label, expected, amount) -> assertEq label expected (toPaise amount)) paiseCases
  assertEq "rupees convert to paise" 50000 (toPaise rupees)
  assertEq "crores convert to paise" 5000000000 (toPaise (fromCrores 5))
  assertEq "money addition" (Right (fromRupees 700)) (moneyAdd rupees (fromRupees 200))
  assertEq "money subtraction" (Right (fromRupees 300)) (moneySubtract rupees (fromRupees 200))
  assertEq
    "money addition rejects currency mismatch"
    True
    (case moneyAdd rupees (mkMoney USD 1) of Left _ -> True; Right _ -> False)
  assertEq
    "money subtraction rejects currency mismatch"
    True
    (case moneySubtract rupees (mkMoney USD 1) of Left _ -> True; Right _ -> False)
  assertEq
    "money ratio rejects zero denominator"
    True
    (case moneyRatio rupees zeroMoney of Left _ -> True; Right _ -> False)
  assertEq
    "money ratio rejects currency mismatch"
    True
    (case moneyRatio rupees (mkMoney USD 100) of Left _ -> True; Right _ -> False)
  assertEq
    "exact rational scaling rounds half paise using ties-to-even"
    (fromPaise 2)
    (moneyScale (1 % 2) (fromPaise 3))
  assertEq
    "negative exact rational scaling"
    (fromPaise (-2))
    (moneyScale (1 % 2) (fromPaise (-3)))
  assertEq
    "money sign distinguishes negative, zero, positive"
    [LT, EQ, GT]
    (map moneySign [fromPaise (-1), zeroMoney, fromPaise 1])

  let nonFinite = [0 / 0, 1 / 0, -(1 / 0)]
  assertTrue "mkRate rejects NaN and both infinities" (all (isLeft . mkRate) nonFinite)
  assertTrue "mkMultiple rejects NaN and both infinities" (all (isLeft . mkMultiple) nonFinite)
  assertEq "mkRate accepts negative rates" True (isRight (mkRate (-1.0)))
  assertEq "mkRate accepts zero" True (isRight (mkRate 0.0))
  assertEq "mkMultiple accepts zero" True (isRight (mkMultiple 0.0))
  assertEq "positive years rejects zero" True (isLeft (mkPositiveYears 0))
  assertEq "positive years rejects negative duration" True (isLeft (mkPositiveYears (-1)))
  assertTrue "positive years rejects non-finite durations" (all (isLeft . mkPositiveYears) nonFinite)
  assertEq "positive years accepts smallest positive duration" True (isRight (mkPositiveYears 1e-300))

  fiscalYear2024 <- expectRight "construct FY 2024" (mkFiscalYear 2024)
  fiscalYear2000 <- expectRight "construct FY 2000" (mkFiscalYear 2000)
  let fiscalCases =
        [ (fiscalYear2024, "2023-04-01", "2024-03-31")
        , (fiscalYear2000, "1999-04-01", "2000-03-31")
        ]
  mapM_
    ( \(year, expectedStart, expectedEnd) -> do
        let (startDay, endDay) = fiscalYearBounds year
        assertEq ("fiscal year start " ++ show year) expectedStart (show startDay)
        assertEq ("fiscal year end " ++ show year) expectedEnd (show endDay)
    )
    fiscalCases
  assertEq "fiscal year rejects zero" True (isLeft (mkFiscalYear 0))
  assertEq "fiscal year rejects negative labels" True (isLeft (mkFiscalYear (-1)))
  assertEq
    "custom date range rejects reversed dates"
    True
    (isLeft (mkDateRange (readDay 2024 1 2) (readDay 2024 1 1)))
  assertEq
    "custom date range permits a single day"
    True
    (isRight (mkDateRange (readDay 2024 1 1) (readDay 2024 1 1)))
  assertEq "year fraction rejects negative duration" True (isLeft (mkYearFraction (-0.1)))
  assertTrue "year fraction rejects non-finite duration" (all (isLeft . mkYearFraction) nonFinite)
  assertEq "year fraction accepts zero" True (isRight (mkYearFraction 0))
  assertEq
    "year fraction rejects reversed dates"
    True
    (isLeft (diffDaysYearFraction (readDay 2024 1 2) (readDay 2024 1 1)))
  assertEq
    "Actual/365 handles a leap day as elapsed days"
    (Right (1 / 365 :: Double))
    (fmap unYearFraction (diffDaysYearFraction (readDay 2024 2 28) (readDay 2024 2 29)))
  assertEq
    "Actual/365 rejects durations that overflow Double"
    True
    (isLeft (diffDaysYearFraction (readDay 1 1 1) (readDay (10 ^ (400 :: Integer)) 1 1)))

  runProperty "Money addition/subtraction round-trips generated integer amounts" prop_moneyRoundTrip
  runProperty "Money ratios preserve exact integer arithmetic" prop_moneyRatioExact
  runProperty "Money scaling rounds exact rationals to paise" prop_moneyScaleExact

  putStrLn "Seby.Domain tests passed successfully."

isLeft :: Either a b -> Bool
isLeft value = case value of
  Left _ -> True
  Right _ -> False

isRight :: Either a b -> Bool
isRight = not . isLeft

expectRight :: (Show e) => String -> Either e a -> IO a
expectRight label value = case value of
  Left err -> do
    putStrLn $ "  [FAIL] " ++ label ++ "\n    Unexpected error: " ++ show err
    exitFailure
  Right result -> pure result

readDay :: Integer -> Int -> Int -> Day
readDay = fromGregorian

runProperty :: (Testable prop) => String -> prop -> IO ()
runProperty label property = do
  result <- quickCheckWithResult stdArgs{maxSuccess = 500} property
  assertTrue label (isSuccess result)

prop_moneyRoundTrip :: Integer -> Integer -> Bool
prop_moneyRoundTrip leftAmount rightAmount =
  let left = fromPaise leftAmount
      right = fromPaise rightAmount
   in case moneyAdd left right of
        Left _ -> False
        Right total -> moneySubtract total right == Right left

prop_moneyRatioExact :: Integer -> NonZero Integer -> Bool
prop_moneyRatioExact numerator (NonZero denominator) =
  case moneyRatio (fromPaise numerator) (fromPaise denominator) of
    Left _ -> False
    Right ratio -> ratio * fromInteger denominator == fromInteger numerator

prop_moneyScaleExact :: Integer -> Integer -> NonZero Integer -> Bool
prop_moneyScaleExact amount factorNumerator (NonZero factorDenominator) =
  let factor = factorNumerator % factorDenominator
      expected = fromPaise (round (fromInteger amount * factor))
   in moneyScale factor (fromPaise amount) == expected

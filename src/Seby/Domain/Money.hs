{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Exact, discrete monetary representation for financial accounting.
Internally uses integer paise (minor units: 1 INR = 100 paise)
to eliminate IEEE 754 floating-point rounding errors.
-}
module Seby.Domain.Money (
  Money,
  Currency (..),
  mkMoney,
  moneyCurrency,
  fromPaise,
  toPaise,
  fromRupees,
  toRupeesDouble,
  fromCrores,
  toCroresDouble,
  zeroMoney,
  moneyAdd,
  moneySubtract,
  moneyScale,
  moneyRatio,
  moneySign,
) where

import Data.Ratio (Ratio, (%))
import Data.Text (Text)

-- | Currency ISO-4217 code.
data Currency = INR | USD | EUR
  deriving stock (Eq, Ord, Show, Enum, Bounded)

{- | Exact monetary amount in integer minor units (paise for INR).
No Fractional instance is provided to prevent illegal dimensional multiplication.
-}
data Money = Money
  { moneyCurrency :: !Currency
  , moneyPaise :: !Integer
  }
  deriving stock (Eq, Ord, Show)

-- | Smart constructor for Money in given currency and integer minor units.
mkMoney :: Currency -> Integer -> Money
mkMoney = Money

-- | Construct Money in INR from integer paise.
fromPaise :: Integer -> Money
fromPaise = Money INR

-- | Extract total paise.
toPaise :: Money -> Integer
toPaise = moneyPaise

-- | Construct Money in INR from whole rupees.
fromRupees :: Integer -> Money
fromRupees r = Money INR (r * 100)

-- | Convert Money to rupees as Double (strictly for display/rendering).
toRupeesDouble :: Money -> Double
toRupeesDouble m = fromIntegral (moneyPaise m) / 100.0

-- | Construct Money in INR from Crores (1 Crore = 10,000,000 INR = 1,000,000,000 paise).
fromCrores :: Integer -> Money
fromCrores cr = Money INR (cr * 1000000000)

-- | Convert Money to Crores as Double (strictly for display/rendering).
toCroresDouble :: Money -> Double
toCroresDouble m = fromIntegral (moneyPaise m) / 1000000000.0

-- | Zero money in INR.
zeroMoney :: Money
zeroMoney = Money INR 0

-- | Currency-safe addition.
moneyAdd :: Money -> Money -> Either Text Money
moneyAdd (Money c1 p1) (Money c2 p2)
  | c1 /= c2 = Left "Cannot add monetary amounts of different currencies"
  | otherwise = Right (Money c1 (p1 + p2))

-- | Currency-safe subtraction.
moneySubtract :: Money -> Money -> Either Text Money
moneySubtract (Money c1 p1) (Money c2 p2)
  | c1 /= c2 = Left "Cannot subtract monetary amounts of different currencies"
  | otherwise = Right (Money c1 (p1 - p2))

-- | Dimensionless scalar scaling.
moneyScale :: Rational -> Money -> Money
moneyScale factor (Money c p) =
  Money c (round (fromInteger p * factor))

-- | Exact dimensionless ratio of two monetary amounts in the same currency.
moneyRatio :: Money -> Money -> Either Text (Ratio Integer)
moneyRatio (Money c1 p1) (Money c2 p2)
  | c1 /= c2 = Left "Cannot compute ratio across different currencies"
  | p2 == 0 = Left "Division by zero monetary denominator"
  | otherwise = Right (p1 % p2)

-- | Determine whether money is strictly positive, negative, or zero.
moneySign :: Money -> Ordering
moneySign (Money _ p) = compare p 0

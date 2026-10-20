{-# LANGUAGE DerivingStrategies #-}

-- | Validated daily OHLCV observations for an identified listed security.
module Seby.Market.Prices (
  DailyPrice,
  mkDailyPrice,
  dailyPriceCurrency,
  dailyPriceDate,
  dailyPriceOpen,
  dailyPriceHigh,
  dailyPriceLow,
  dailyPriceClose,
  dailyPriceVolume,
  PriceError (..),
) where

import Data.Ratio (Ratio)
import Data.Time.Calendar (Day)
import Seby.Domain.Money (Currency)

data DailyPrice
  = DailyPrice
      !Currency
      !Day
      !(Ratio Integer)
      !(Ratio Integer)
      !(Ratio Integer)
      !(Ratio Integer)
      !Integer
  deriving stock (Eq, Show)

data PriceError
  = NonPositivePrice
  | InvalidPriceRange
  | NegativeVolume
  deriving stock (Eq, Show)

-- | Construct a quote only when OHLC values form a valid positive range.
mkDailyPrice ::
  Currency ->
  Day ->
  Ratio Integer ->
  Ratio Integer ->
  Ratio Integer ->
  Ratio Integer ->
  Integer ->
  Either PriceError DailyPrice
mkDailyPrice currency day open high low close volume
  | any (<= 0) [open, high, low, close] = Left NonPositivePrice
  | low > high || open < low || open > high || close < low || close > high = Left InvalidPriceRange
  | volume < 0 = Left NegativeVolume
  | otherwise = Right (DailyPrice currency day open high low close volume)

dailyPriceCurrency :: DailyPrice -> Currency
dailyPriceCurrency (DailyPrice currency _ _ _ _ _ _) = currency

dailyPriceDate :: DailyPrice -> Day
dailyPriceDate (DailyPrice _ day _ _ _ _ _) = day

dailyPriceOpen :: DailyPrice -> Ratio Integer
dailyPriceOpen (DailyPrice _ _ value _ _ _ _) = value

dailyPriceHigh :: DailyPrice -> Ratio Integer
dailyPriceHigh (DailyPrice _ _ _ value _ _ _) = value

dailyPriceLow :: DailyPrice -> Ratio Integer
dailyPriceLow (DailyPrice _ _ _ _ value _ _) = value

dailyPriceClose :: DailyPrice -> Ratio Integer
dailyPriceClose (DailyPrice _ _ _ _ _ value _) = value

dailyPriceVolume :: DailyPrice -> Integer
dailyPriceVolume (DailyPrice _ _ _ _ _ _ value) = value

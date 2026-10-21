{-# LANGUAGE DerivingStrategies #-}

{- | Deterministic portfolio snapshots from caller-supplied positions and dated
market prices. Values are grouped by currency; no foreign-exchange conversion,
tax, fees, dividends, or corporate-action adjustments are inferred here.
-}
module Seby.Analysis.Portfolio (
  PortfolioPosition,
  mkPortfolioPosition,
  positionSymbol,
  positionQuantity,
  positionAverageCost,
  positionMarketPrice,
  positionPriceDate,
  positionPriceProvenance,
  PortfolioError (..),
  PositionSummary (..),
  CurrencySummary (..),
  summarizePortfolio,
) where

import Control.Monad (when)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Ratio (Ratio)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Seby.Domain.Money (
  Currency,
  Money,
  moneyAdd,
  moneyCurrency,
  moneyRatio,
  moneyScale,
  moneySign,
  moneySubtract,
 )
import Seby.Domain.Types (Provenance, Symbol)

{- | One position with an average per-share acquisition cost and a dated
per-share market mark. Quantity may be fractional.
-}
data PortfolioPosition = PortfolioPosition !Symbol !(Ratio Integer) !Money !Money !Day !Provenance
  deriving stock (Eq, Show)

data PortfolioError
  = NonPositivePositionQuantity !Symbol
  | NonPositivePositionCost !Symbol
  | NonPositivePositionMarketPrice !Symbol
  | PositionValueRoundsToZero !Symbol
  | PositionCurrencyMismatch !Symbol
  | DuplicatePortfolioSymbol !Symbol
  | EmptyPortfolio
  | PortfolioArithmeticError !Text
  deriving stock (Eq, Show)

mkPortfolioPosition ::
  Symbol ->
  Ratio Integer ->
  Money ->
  Money ->
  Day ->
  Provenance ->
  Either PortfolioError PortfolioPosition
mkPortfolioPosition symbol quantity averageCost marketPrice priceDate provenance
  | quantity <= 0 = Left (NonPositivePositionQuantity symbol)
  | moneySign averageCost /= GT = Left (NonPositivePositionCost symbol)
  | moneySign marketPrice /= GT = Left (NonPositivePositionMarketPrice symbol)
  | moneyCurrency averageCost /= moneyCurrency marketPrice = Left (PositionCurrencyMismatch symbol)
  | otherwise = Right (PortfolioPosition symbol quantity averageCost marketPrice priceDate provenance)

positionSymbol :: PortfolioPosition -> Symbol
positionSymbol (PortfolioPosition symbol _ _ _ _ _) = symbol

positionQuantity :: PortfolioPosition -> Ratio Integer
positionQuantity (PortfolioPosition _ quantity _ _ _ _) = quantity

positionAverageCost :: PortfolioPosition -> Money
positionAverageCost (PortfolioPosition _ _ averageCost _ _ _) = averageCost

positionMarketPrice :: PortfolioPosition -> Money
positionMarketPrice (PortfolioPosition _ _ _ marketPrice _ _) = marketPrice

positionPriceDate :: PortfolioPosition -> Day
positionPriceDate (PortfolioPosition _ _ _ _ priceDate _) = priceDate

positionPriceProvenance :: PortfolioPosition -> Provenance
positionPriceProvenance (PortfolioPosition _ _ _ _ _ provenance) = provenance

data PositionSummary = PositionSummary
  { summarySymbol :: !Symbol
  , summaryQuantity :: !(Ratio Integer)
  , summaryAverageCost :: !Money
  , summaryMarketPrice :: !Money
  , summaryPriceDate :: !Day
  , summaryPriceProvenance :: !Provenance
  , summaryCostBasis :: !Money
  , summaryMarketValue :: !Money
  , summaryUnrealizedGain :: !Money
  , summaryReturnOnCost :: !(Ratio Integer)
  , summaryPortfolioWeight :: !(Ratio Integer)
  }
  deriving stock (Eq, Show)

{- | Holdings totals for one currency. Ratios in each position are fractions
(for example, 1/10 is a 10% return or portfolio weight).
-}
data CurrencySummary = CurrencySummary
  { summaryCurrency :: !Currency
  , summaryPositions :: ![PositionSummary]
  , summaryTotalCostBasis :: !Money
  , summaryTotalMarketValue :: !Money
  , summaryTotalUnrealizedGain :: !Money
  }
  deriving stock (Eq, Show)

{- | Aggregate positions by currency, with stable symbol ordering and weights
calculated only within each currency bucket.
-}
summarizePortfolio :: [PortfolioPosition] -> Either PortfolioError [CurrencySummary]
summarizePortfolio [] = Left EmptyPortfolio
summarizePortfolio positions = do
  ensureUniqueSymbols positions
  let buckets =
        Map.fromListWith
          (<>)
          [(moneyCurrency (positionMarketPrice position), [position]) | position <- positions]
  traverse (uncurry summarizeCurrency) (Map.toAscList buckets)

ensureUniqueSymbols :: [PortfolioPosition] -> Either PortfolioError ()
ensureUniqueSymbols = go Set.empty
  where
    go _ [] = Right ()
    go seen (position : remaining)
      | Set.member symbol seen = Left (DuplicatePortfolioSymbol symbol)
      | otherwise = go (Set.insert symbol seen) remaining
      where
        symbol = positionSymbol position

summarizeCurrency :: Currency -> [PortfolioPosition] -> Either PortfolioError CurrencySummary
summarizeCurrency currency positions = do
  rows <- traverse summarizePosition (sortOn positionSymbol positions)
  totalCost <- sumMoney (map summaryCostBasis rows)
  totalValue <- sumMoney (map summaryMarketValue rows)
  totalGain <- moneyArithmetic (moneySubtract totalValue totalCost)
  weightedRows <- traverse (setWeight totalValue) rows
  pure
    CurrencySummary
      { summaryCurrency = currency
      , summaryPositions = weightedRows
      , summaryTotalCostBasis = totalCost
      , summaryTotalMarketValue = totalValue
      , summaryTotalUnrealizedGain = totalGain
      }

summarizePosition :: PortfolioPosition -> Either PortfolioError PositionSummary
summarizePosition position = do
  let quantity = positionQuantity position
      costBasis = moneyScale quantity (positionAverageCost position)
      marketValue = moneyScale quantity (positionMarketPrice position)
  when
    (moneySign costBasis /= GT || moneySign marketValue /= GT)
    (Left (PositionValueRoundsToZero (positionSymbol position)))
  gain <- moneyArithmetic (moneySubtract marketValue costBasis)
  returnOnCost <- moneyArithmetic (moneyRatio gain costBasis)
  pure
    PositionSummary
      { summarySymbol = positionSymbol position
      , summaryQuantity = quantity
      , summaryAverageCost = positionAverageCost position
      , summaryMarketPrice = positionMarketPrice position
      , summaryPriceDate = positionPriceDate position
      , summaryPriceProvenance = positionPriceProvenance position
      , summaryCostBasis = costBasis
      , summaryMarketValue = marketValue
      , summaryUnrealizedGain = gain
      , summaryReturnOnCost = returnOnCost
      , summaryPortfolioWeight = 0
      }

setWeight :: Money -> PositionSummary -> Either PortfolioError PositionSummary
setWeight totalValue row = do
  weight <- moneyArithmetic (moneyRatio (summaryMarketValue row) totalValue)
  pure row{summaryPortfolioWeight = weight}

sumMoney :: [Money] -> Either PortfolioError Money
sumMoney [] = Left EmptyPortfolio
sumMoney (first : rest) = foldl add (Right first) rest
  where
    add accumulated value = do
      current <- accumulated
      moneyArithmetic (moneyAdd current value)

moneyArithmetic :: Either Text b -> Either PortfolioError b
moneyArithmetic = either (Left . PortfolioArithmeticError) Right

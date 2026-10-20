{-# LANGUAGE DerivingStrategies #-}

{- | Segment revenue comparison that preserves the disclosure and reconciliation
state. A segment concentration index is exposed only for a complete, reconciled
set with non-negative segment revenue.
-}
module Seby.Analysis.Segments (
  SegmentRevenue,
  mkSegmentRevenue,
  segmentRevenueName,
  segmentRevenueAmount,
  segmentRevenueProvenance,
  SegmentDisclosure (..),
  SegmentError (..),
  SegmentShare (..),
  SegmentConcentration (..),
  SegmentAnalysis (..),
  analyzeSegments,
) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Ratio (Ratio)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Seby.Domain.Money (Money, moneyAdd, moneyCurrency, moneyRatio, moneySign, moneySubtract)
import Seby.Domain.Period (FiscalPeriod)
import Seby.Domain.Types (CompanyId, ConsolidationScope, Provenance)

data SegmentRevenue = SegmentRevenue !Text !Money !(NonEmpty Provenance)
  deriving stock (Eq, Show)

mkSegmentRevenue :: Text -> Money -> NonEmpty Provenance -> Either SegmentError SegmentRevenue
mkSegmentRevenue name amount provenance
  | Text.null (Text.strip name) = Left BlankSegmentName
  | otherwise = Right (SegmentRevenue (Text.strip name) amount provenance)

segmentRevenueName :: SegmentRevenue -> Text
segmentRevenueName (SegmentRevenue name _ _) = name

segmentRevenueAmount :: SegmentRevenue -> Money
segmentRevenueAmount (SegmentRevenue _ amount _) = amount

segmentRevenueProvenance :: SegmentRevenue -> NonEmpty Provenance
segmentRevenueProvenance (SegmentRevenue _ _ provenance) = provenance

data SegmentDisclosure = CompleteDisclosure | PartialDisclosure
  deriving stock (Eq, Ord, Show)

data SegmentError
  = BlankSegmentName
  | DuplicateSegmentName !Text
  | NonPositiveGroupRevenue
  | SegmentCurrencyMismatch
  | SegmentTotalCurrencyMismatch
  deriving stock (Eq, Show)

data SegmentShare = SegmentShare
  { segmentShareName :: !Text
  , segmentShareAmount :: !Money
  , segmentShareOfGroupRevenue :: !(Ratio Integer)
  , segmentShareEvidence :: !(NonEmpty Provenance)
  }
  deriving stock (Eq, Show)

data SegmentConcentration
  = HhiAvailable !(Ratio Integer)
  | HhiUnavailablePartialDisclosure
  | HhiUnavailableUnreconciled
  | HhiUnavailableNegativeSegmentRevenue
  | HhiUnavailableNonPositiveSegmentTotal
  deriving stock (Eq, Show)

data SegmentAnalysis = SegmentAnalysis
  { segmentAnalysisCompany :: !CompanyId
  , segmentAnalysisPeriod :: !FiscalPeriod
  , segmentAnalysisScope :: !ConsolidationScope
  , segmentAnalysisGroupRevenue :: !Money
  , segmentAnalysisDisclosedRevenue :: !Money
  , segmentAnalysisReconciliationDifference :: !Money
  , segmentAnalysisShares :: !(NonEmpty SegmentShare)
  , segmentAnalysisConcentration :: !SegmentConcentration
  , segmentAnalysisDisclosure :: !SegmentDisclosure
  }
  deriving stock (Eq, Show)

analyzeSegments ::
  CompanyId ->
  FiscalPeriod ->
  ConsolidationScope ->
  Money ->
  SegmentDisclosure ->
  NonEmpty SegmentRevenue ->
  Either SegmentError SegmentAnalysis
analyzeSegments company period scope groupRevenue disclosure segments = do
  if moneySign groupRevenue == GT
    then pure ()
    else Left NonPositiveGroupRevenue
  checkUniqueNames segments
  if all ((== moneyCurrency groupRevenue) . moneyCurrency . segmentRevenueAmount) segments
    then pure ()
    else Left SegmentCurrencyMismatch
  segmentTotal <- sumSegments segments
  difference <- case moneySubtract segmentTotal groupRevenue of
    Left _ -> Left SegmentTotalCurrencyMismatch
    Right amount -> Right amount
  shares <- traverse (toShare groupRevenue) segments
  case shares of
    firstShare :| restShares ->
      Right
        SegmentAnalysis
          { segmentAnalysisCompany = company
          , segmentAnalysisPeriod = period
          , segmentAnalysisScope = scope
          , segmentAnalysisGroupRevenue = groupRevenue
          , segmentAnalysisDisclosedRevenue = segmentTotal
          , segmentAnalysisReconciliationDifference = difference
          , segmentAnalysisShares = firstShare :| restShares
          , segmentAnalysisConcentration = concentration disclosure difference segmentTotal segments
          , segmentAnalysisDisclosure = disclosure
          }
  where
    toShare total segment = case moneyRatio (segmentRevenueAmount segment) total of
      Left _ -> Left SegmentTotalCurrencyMismatch
      Right share ->
        Right
          SegmentShare
            { segmentShareName = segmentRevenueName segment
            , segmentShareAmount = segmentRevenueAmount segment
            , segmentShareOfGroupRevenue = share
            , segmentShareEvidence = segmentRevenueProvenance segment
            }

checkUniqueNames :: NonEmpty SegmentRevenue -> Either SegmentError ()
checkUniqueNames = go Set.empty . toList
  where
    go _ [] = Right ()
    go seen (segment : rest)
      | Set.member name seen = Left (DuplicateSegmentName name)
      | otherwise = go (Set.insert name seen) rest
      where
        name = segmentRevenueName segment

sumSegments :: NonEmpty SegmentRevenue -> Either SegmentError Money
sumSegments (first :| rest) = foldl addSegment (Right (segmentRevenueAmount first)) rest
  where
    addSegment accumulated segment = case accumulated of
      Left err -> Left err
      Right total -> case moneyAdd total (segmentRevenueAmount segment) of
        Left _ -> Left SegmentCurrencyMismatch
        Right next -> Right next

concentration ::
  SegmentDisclosure -> Money -> Money -> NonEmpty SegmentRevenue -> SegmentConcentration
concentration PartialDisclosure _ _ _ = HhiUnavailablePartialDisclosure
concentration CompleteDisclosure difference segmentTotal segments
  | any ((== LT) . moneySign . segmentRevenueAmount) (toList segments) =
      HhiUnavailableNegativeSegmentRevenue
  | moneySign segmentTotal /= GT = HhiUnavailableNonPositiveSegmentTotal
  | moneySign difference /= EQ = HhiUnavailableUnreconciled
  | otherwise = case traverse squaredShare (toList segments) of
      Left _ -> HhiUnavailableUnreconciled
      Right shares -> HhiAvailable (sum shares)
  where
    squaredShare segment = do
      share <- moneyRatio (segmentRevenueAmount segment) segmentTotal
      pure (share * share)

toList :: NonEmpty a -> [a]
toList (first :| rest) = first : rest

{-# LANGUAGE DerivingStrategies #-}

{- | Transparent peer selection and small-sample industry summaries.

Peer ranking is a documented heuristic: eligibility requires matching industry,
period, statement basis, and currency; similarity then rewards business model,
market capitalisation, and revenue closeness. A score is not a probability.
-}
module Seby.Analysis.Peers (
  Industry (..),
  BusinessModel (..),
  CompanyProfile,
  mkCompanyProfile,
  profileCompanyId,
  profileName,
  profileIndustry,
  PeerInputError (..),
  PeerExclusion (..),
  PeerReason (..),
  PeerCandidate (..),
  PeerSelection (..),
  rankPeers,
  minimumIndustrySample,
  IndustrySummaryError (..),
  medianRate,
  AnalysisMetric (..),
  IndustryApplicability (..),
  metricApplicability,
) where

import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Ord (Down (..), comparing)
import Data.Ratio (Ratio, (%))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Seby.Domain.Money (Money, moneyCurrency, moneySign, toPaise)
import Seby.Domain.Period (FiscalPeriod)
import Seby.Domain.Types (
  CompanyId,
  Rate,
  StatementBasis,
  mkRate,
  unRate,
 )

-- | Coarse industry classes used only by applicability policy.
data Industry
  = GeneralIndustry !Text
  | Bank
  | Insurance
  | Utility
  deriving stock (Eq, Ord, Show)

-- | Business traits supplied by the analyst; tags are not inferred from ratios.
data BusinessModel
  = BusinessToBusiness
  | BusinessToConsumer
  | AssetHeavy
  | Regulated
  | SubscriptionBased
  | CommodityExposed
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data CompanyProfile = CompanyProfile
  { profileCompanyId :: !CompanyId
  , profileName :: !Text
  , profileIndustry :: !Industry
  , profileBusinessModels :: !(Set BusinessModel)
  , profilePeriod :: !FiscalPeriod
  , profileStatementBasis :: !StatementBasis
  , profileRevenue :: !Money
  , profileMarketCapitalisation :: !Money
  }
  deriving stock (Eq, Show)

data PeerInputError
  = BlankCompanyName
  | BlankIndustryName
  | NonPositiveRevenue
  | NonPositiveMarketCapitalisation
  | ProfileCurrencyMismatch
  deriving stock (Eq, Show)

-- | Create a profile only when its identifying and size inputs are usable.
mkCompanyProfile ::
  CompanyId ->
  Text ->
  Industry ->
  NonEmpty BusinessModel ->
  FiscalPeriod ->
  StatementBasis ->
  Money ->
  Money ->
  Either PeerInputError CompanyProfile
mkCompanyProfile identifier name industry models period basis revenue marketCap
  | Text.null (Text.strip name) = Left BlankCompanyName
  | isBlankIndustry industry = Left BlankIndustryName
  | moneySign revenue /= GT = Left NonPositiveRevenue
  | moneySign marketCap /= GT = Left NonPositiveMarketCapitalisation
  | moneyCurrency revenue /= moneyCurrency marketCap = Left ProfileCurrencyMismatch
  | otherwise =
      Right
        CompanyProfile
          { profileCompanyId = identifier
          , profileName = Text.strip name
          , profileIndustry = industry
          , profileBusinessModels = Set.fromList (toList models)
          , profilePeriod = period
          , profileStatementBasis = basis
          , profileRevenue = revenue
          , profileMarketCapitalisation = marketCap
          }
  where
    isBlankIndustry (GeneralIndustry label) = Text.null (Text.strip label)
    isBlankIndustry _ = False
    toList (first :| rest) = first : rest

data PeerExclusion
  = SameCompany
  | DifferentIndustry
  | DifferentPeriod
  | DifferentStatementBasis
  | DifferentCurrency
  deriving stock (Eq, Show)

data PeerReason
  = SameIndustry
  | BusinessModelOverlap !(Ratio Integer)
  | MarketCapitalisationCloseness !(Ratio Integer)
  | RevenueCloseness !(Ratio Integer)
  deriving stock (Eq, Show)

data PeerCandidate = PeerCandidate
  { candidateProfile :: !CompanyProfile
  , candidateScore :: !(Ratio Integer)
  , candidateReasons :: ![PeerReason]
  }
  deriving stock (Eq, Show)

data PeerSelection = PeerSelection
  { selectedPeers :: ![PeerCandidate]
  , excludedPeers :: ![(CompanyProfile, PeerExclusion)]
  }
  deriving stock (Eq, Show)

{- | Rank all eligible peers deterministically and preserve every exclusion.

Eligible profiles must share company industry, fiscal period, statement basis,
and currency. Score = 40 industry points + 25×business-model Jaccard similarity
+ 20×market-cap similarity + 15×revenue similarity. Each size similarity is
min(amounts)/max(amounts); the exact rational score is a ranking aid, not a
probability. Market capitalisation inputs should be measured at the same date.
-}
rankPeers :: CompanyProfile -> [CompanyProfile] -> PeerSelection
rankPeers target candidates =
  PeerSelection
    { selectedPeers =
        sortBy
          (comparing (Down . candidateScore) <> comparing (profileCompanyId . candidateProfile))
          eligible
    , excludedPeers = exclusions
    }
  where
    (eligible, exclusions) = foldr classify ([], []) candidates
    classify candidate (peers, rejected) = case exclusionReason target candidate of
      Just reason -> (peers, (candidate, reason) : rejected)
      Nothing -> (scorePeer target candidate : peers, rejected)

exclusionReason :: CompanyProfile -> CompanyProfile -> Maybe PeerExclusion
exclusionReason target candidate
  | profileCompanyId target == profileCompanyId candidate = Just SameCompany
  | profileIndustry target /= profileIndustry candidate = Just DifferentIndustry
  | profilePeriod target /= profilePeriod candidate = Just DifferentPeriod
  | profileStatementBasis target /= profileStatementBasis candidate = Just DifferentStatementBasis
  | moneyCurrency (profileRevenue target) /= moneyCurrency (profileRevenue candidate) =
      Just DifferentCurrency
  | otherwise = Nothing

scorePeer :: CompanyProfile -> CompanyProfile -> PeerCandidate
scorePeer target candidate =
  PeerCandidate
    { candidateProfile = candidate
    , candidateScore = 40 + 25 * modelSimilarity + 20 * capSimilarity + 15 * revenueSimilarity
    , candidateReasons =
        [ SameIndustry
        , BusinessModelOverlap modelSimilarity
        , MarketCapitalisationCloseness capSimilarity
        , RevenueCloseness revenueSimilarity
        ]
    }
  where
    modelSimilarity = jaccard (profileBusinessModels target) (profileBusinessModels candidate)
    capSimilarity = magnitudeSimilarity (profileMarketCapitalisation target) (profileMarketCapitalisation candidate)
    revenueSimilarity = magnitudeSimilarity (profileRevenue target) (profileRevenue candidate)

-- | Jaccard similarity is the intersection size divided by union size.
jaccard :: Set BusinessModel -> Set BusinessModel -> Ratio Integer
jaccard left right =
  toInteger (Set.size (Set.intersection left right)) % toInteger (Set.size (Set.union left right))

-- | Similarity is min(value)/max(value), in [0,1] for positive values.
magnitudeSimilarity :: Money -> Money -> Ratio Integer
magnitudeSimilarity left right =
  min (abs (toPaise left)) (abs (toPaise right)) % max (abs (toPaise left)) (abs (toPaise right))

minimumIndustrySample :: Int
minimumIndustrySample = 5

data IndustrySummaryError
  = TooFewIndustryObservations !Int !Int
  | InvalidIndustryMedian
  deriving stock (Eq, Show)

{- | Median rate is available only at the minimum sample size; even samples use
the arithmetic mean of the two central observations.
-}
medianRate :: [Rate] -> Either IndustrySummaryError Rate
medianRate rates
  | sampleSize < minimumIndustrySample =
      Left (TooFewIndustryObservations minimumIndustrySample sampleSize)
  | otherwise = case mkRate medianValue of
      Left _ -> Left InvalidIndustryMedian
      Right median -> Right median
  where
    ordered = sortBy (comparing unRate) rates
    sampleSize = length ordered
    middle = sampleSize `div` 2
    medianValue = case splitAt middle ordered of
      (_, upper : _) | odd sampleSize -> unRate upper
      (lower, upper : _) -> case reverse lower of
        lowerMiddle : _ -> unRate lowerMiddle / 2 + unRate upper / 2
        [] -> 0
      _ -> 0

data AnalysisMetric
  = ReturnOnEquityMetric
  | ReturnOnCapitalMetric
  | DebtToEquityMetric
  | InterestCoverageMetric
  | CurrentRatioMetric
  | CashConversionMetric
  | EVToEBITDAMetric
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data IndustryApplicability
  = Applicable
  | NotApplicableInIndustry !Text
  deriving stock (Eq, Show)

-- | Avoid applying generic industrial-company ratios to financial institutions.
metricApplicability :: Industry -> AnalysisMetric -> IndustryApplicability
metricApplicability industry metric = case (industry, metric) of
  (Bank, ReturnOnCapitalMetric) -> notApplicable "Capital-employed definitions are not comparable for banks"
  (Insurance, ReturnOnCapitalMetric) -> notApplicable "Capital-employed definitions are not comparable for insurers"
  (Bank, DebtToEquityMetric) -> notApplicable "Deposits and regulatory capital make debt/equity misleading for banks"
  (Insurance, DebtToEquityMetric) -> notApplicable "Insurance liabilities make debt/equity misleading for insurers"
  (Bank, InterestCoverageMetric) -> notApplicable "Interest is an operating input for banks"
  (Insurance, InterestCoverageMetric) -> notApplicable "Interest coverage is not a standard insurer solvency measure"
  (Bank, CurrentRatioMetric) -> notApplicable "Liquidity is assessed differently for banks"
  (Insurance, CurrentRatioMetric) -> notApplicable "Liquidity is assessed differently for insurers"
  (Bank, CashConversionMetric) -> notApplicable "Operating cash conversion is not comparable for banks"
  (Insurance, CashConversionMetric) -> notApplicable "Operating cash conversion is not comparable for insurers"
  (Bank, EVToEBITDAMetric) -> notApplicable "EV/EBITDA is not a suitable bank valuation multiple"
  (Insurance, EVToEBITDAMetric) -> notApplicable "EV/EBITDA is not a suitable insurer valuation multiple"
  _ -> Applicable
  where
    notApplicable = NotApplicableInIndustry

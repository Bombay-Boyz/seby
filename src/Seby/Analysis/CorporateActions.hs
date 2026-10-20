{-# LANGUAGE DerivingStrategies #-}

{- | Provenance-backed corporate-action history and cautious per-share views.

Only splits and bonus issues have automatic mechanical price adjustments.
Rights issues, mergers, demergers, and consolidation changes remain explicitly
non-comparable until an analyst supplies a documented adjustment.
-}
module Seby.Analysis.CorporateActions (
  CorporateActionType (..),
  CorporateAction,
  CorporateActionError (..),
  mkCorporateAction,
  corporateActionId,
  corporateActionCompany,
  corporateActionEffectiveDate,
  corporateActionType,
  corporateActionProvenance,
  ActionLedger,
  mkActionLedger,
  ledgerActions,
  actionsKnownAt,
  SharePriceView (..),
  SharePriceAdjustment (..),
  ComparabilityIssue (..),
  SharePriceViewResult (..),
  viewHistoricalSharePrice,
) where

import Data.List (sortBy)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Ratio (Ratio)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, UTCTime, utctDay)
import Seby.Domain.Money (Currency)
import Seby.Domain.Types (CompanyId, Provenance (..))
import Seby.Market.Prices (DailyPrice, dailyPriceClose, dailyPriceCurrency, dailyPriceDate)

data CorporateActionType
  = ShareSplit !(Ratio Integer)
  | BonusIssue !(Ratio Integer)
  | RightsIssue
  | Merger
  | Demerger
  | ConsolidationScopeChange
  | ShareIssuance
  | Buyback
  deriving stock (Eq, Show)

data CorporateAction = CorporateAction !Text !CompanyId !Day !CorporateActionType !Provenance
  deriving stock (Eq, Show)

data CorporateActionError
  = BlankCorporateActionId
  | InvalidCorporateActionRatio !CorporateActionType
  | DuplicateCorporateActionId !Text
  deriving stock (Eq, Show)

mkCorporateAction ::
  Text ->
  CompanyId ->
  Day ->
  CorporateActionType ->
  Provenance ->
  Either CorporateActionError CorporateAction
mkCorporateAction identifier company effectiveDate actionType provenance
  | Text.null (Text.strip identifier) = Left BlankCorporateActionId
  | invalidRatio actionType = Left (InvalidCorporateActionRatio actionType)
  | otherwise =
      Right (CorporateAction (Text.strip identifier) company effectiveDate actionType provenance)
  where
    invalidRatio (ShareSplit ratio) = ratio <= 0
    invalidRatio (BonusIssue ratio) = ratio <= 0
    invalidRatio _ = False

corporateActionId :: CorporateAction -> Text
corporateActionId (CorporateAction identifier _ _ _ _) = identifier

corporateActionCompany :: CorporateAction -> CompanyId
corporateActionCompany (CorporateAction _ company _ _ _) = company

corporateActionEffectiveDate :: CorporateAction -> Day
corporateActionEffectiveDate (CorporateAction _ _ effectiveDate _ _) = effectiveDate

corporateActionType :: CorporateAction -> CorporateActionType
corporateActionType (CorporateAction _ _ _ actionType _) = actionType

corporateActionProvenance :: CorporateAction -> Provenance
corporateActionProvenance (CorporateAction _ _ _ _ provenance) = provenance

newtype ActionLedger = ActionLedger [CorporateAction]
  deriving stock (Eq, Show)

mkActionLedger :: [CorporateAction] -> Either CorporateActionError ActionLedger
mkActionLedger actions = do
  ensureUniqueIds actions
  pure . ActionLedger $
    sortBy (comparing corporateActionEffectiveDate <> comparing corporateActionId) actions

ledgerActions :: ActionLedger -> [CorporateAction]
ledgerActions (ActionLedger actions) = actions

ensureUniqueIds :: [CorporateAction] -> Either CorporateActionError ()
ensureUniqueIds = go Map.empty
  where
    go _ [] = Right ()
    go seen (action : rest) =
      let identifier = corporateActionId action
       in if Map.member identifier seen
            then Left (DuplicateCorporateActionId identifier)
            else go (Map.insert identifier () seen) rest

-- | Point-in-time membership requires an explicit publication timestamp.
actionsKnownAt :: UTCTime -> ActionLedger -> [CorporateAction]
actionsKnownAt instant (ActionLedger actions) =
  [ action
  | action <- actions
  , maybe False (<= instant) (provenancePublishedAt (corporateActionProvenance action))
  ]

data SharePriceView
  = AsReported
  | ComparableThrough !Day
  | PointInTimeAt !UTCTime
  deriving stock (Eq, Show)

data SharePriceAdjustment
  = SplitPriceAdjustment !Text !(Ratio Integer)
  | BonusPriceAdjustment !Text !(Ratio Integer)
  deriving stock (Eq, Show)

data ComparabilityIssue
  = RightsIssueNeedsManualAdjustment !Text
  | MergerBreaksPerShareComparability !Text
  | DemergerBreaksPerShareComparability !Text
  | ConsolidationChangeBreaksComparability !Text
  deriving stock (Eq, Show)

data SharePriceViewResult = SharePriceViewResult
  { reportedSharePrice :: !(Ratio Integer)
  , sharePriceCurrency :: !Currency
  , comparableSharePrice :: !(Maybe (Ratio Integer))
  , appliedSharePriceAdjustments :: ![SharePriceAdjustment]
  , sharePriceComparabilityIssues :: ![ComparabilityIssue]
  }
  deriving stock (Eq, Show)

{- | Build a historical price view. Corporate actions only affect observations
strictly before their effective date. Point-in-time views additionally require
the action to have been published by the requested instant. Unsupported
structural adjustments make the comparable value unavailable while retaining
the reported price and the reason.
-}
viewHistoricalSharePrice ::
  SharePriceView -> CompanyId -> DailyPrice -> ActionLedger -> SharePriceViewResult
viewHistoricalSharePrice view company observedPrice ledger =
  let observedAt = dailyPriceDate observedPrice
      reportedPrice = dailyPriceClose observedPrice
      actions = applicableActions view company observedAt ledger
      adjustments = mapMaybeAdjustment actions
      issues = mapMaybeIssue actions
      divisor = product (map adjustmentFactor adjustments)
      adjusted = if null issues then Just (reportedPrice / divisor) else Nothing
   in SharePriceViewResult reportedPrice (dailyPriceCurrency observedPrice) adjusted adjustments issues

applicableActions :: SharePriceView -> CompanyId -> Day -> ActionLedger -> [CorporateAction]
applicableActions AsReported _ _ _ = []
applicableActions (ComparableThrough asOf) company observedAt (ActionLedger actions) =
  filter (effectiveBetween company observedAt asOf) actions
applicableActions (PointInTimeAt instant) company observedAt ledger =
  filter (effectiveBetween company observedAt (utctDay instant)) (actionsKnownAt instant ledger)

effectiveBetween :: CompanyId -> Day -> Day -> CorporateAction -> Bool
effectiveBetween company observedAt asOf action =
  corporateActionCompany action == company
    && corporateActionEffectiveDate action > observedAt
    && corporateActionEffectiveDate action <= asOf

mapMaybeAdjustment :: [CorporateAction] -> [SharePriceAdjustment]
mapMaybeAdjustment = foldr collect []
  where
    collect action rest = case priceAdjustment action of
      Nothing -> rest
      Just adjustment -> adjustment : rest

priceAdjustment :: CorporateAction -> Maybe SharePriceAdjustment
priceAdjustment action = case corporateActionType action of
  ShareSplit ratio -> Just (SplitPriceAdjustment (corporateActionId action) ratio)
  BonusIssue ratio -> Just (BonusPriceAdjustment (corporateActionId action) (1 + ratio))
  _ -> Nothing

adjustmentFactor :: SharePriceAdjustment -> Ratio Integer
adjustmentFactor (SplitPriceAdjustment _ ratio) = ratio
adjustmentFactor (BonusPriceAdjustment _ ratio) = ratio

mapMaybeIssue :: [CorporateAction] -> [ComparabilityIssue]
mapMaybeIssue = foldr collect []
  where
    collect action rest = case comparabilityIssue action of
      Nothing -> rest
      Just issue -> issue : rest

comparabilityIssue :: CorporateAction -> Maybe ComparabilityIssue
comparabilityIssue action = case corporateActionType action of
  RightsIssue -> Just (RightsIssueNeedsManualAdjustment (corporateActionId action))
  Merger -> Just (MergerBreaksPerShareComparability (corporateActionId action))
  Demerger -> Just (DemergerBreaksPerShareComparability (corporateActionId action))
  ConsolidationScopeChange -> Just (ConsolidationChangeBreaksComparability (corporateActionId action))
  _ -> Nothing

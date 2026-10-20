{-# LANGUAGE DerivingStrategies #-}

-- | Persistence boundary values. This module has no database dependencies.
module Seby.Persistence.Types (
  RawPayload (..),
  RawObservation (..),
  CanonicalFact (..),
  RefreshBatch (..),
  RefreshFailure (..),
  RefreshState (..),
  FactPeriod (..),
  StoreError (..),
  CacheMode (..),
  CacheStatus (..),
  cacheStatusAt,
  CacheDecision (..),
  decideCache,
) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (Day, NominalDiffTime, UTCTime, diffUTCTime)
import Seby.Domain.Money (Currency)
import Seby.Domain.Types (CompanyId, PeriodType, Provenance, SourceId, StatementBasis)

{- | Original source bytes and their source/time metadata. The digest is verified
against the bytes before persistence.
-}
data RawPayload = RawPayload
  { rawPayloadProvenance :: !Provenance
  , rawPayloadBytes :: !ByteString
  }
  deriving stock (Eq, Show)

-- | Extracted source value before validation or normalisation.
data RawObservation = RawObservation
  { rawObservationName :: !Text
  , rawObservationValue :: !Text
  , rawObservationUnit :: !Text
  , rawObservationExtractorVersion :: !Text
  , rawObservationPeriodStart :: !Day
  , rawObservationPeriodEnd :: !Day
  }
  deriving stock (Eq, Show)

{- | Validated canonical monetary fact. Each source observation is retained as
an immutable version; a later correction adds another fact row.
-}
data CanonicalFact = CanonicalFact
  { canonicalCompany :: !CompanyId
  , canonicalConcept :: !Text
  , canonicalPeriodStart :: !Day
  , canonicalPeriodEnd :: !Day
  , canonicalPeriodType :: !PeriodType
  , canonicalCurrency :: !Currency
  , canonicalAmountMinor :: !Integer
  , canonicalStatementBasis :: !StatementBasis
  , canonicalScope :: !Text
  , canonicalPolicyVersion :: !Text
  , canonicalProvenance :: !Provenance
  }
  deriving stock (Eq, Show)

-- | Complete validated write unit. All members are committed atomically.
data RefreshBatch = RefreshBatch
  { refreshRunId :: !Text
  , refreshCompany :: !CompanyId
  , refreshCompanyName :: !Text
  , refreshAttemptedAt :: !UTCTime
  , refreshCompletedAt :: !UTCTime
  , refreshPayload :: !RawPayload
  , refreshRawObservations :: ![RawObservation]
  , refreshCanonicalFacts :: ![CanonicalFact]
  }
  deriving stock (Eq, Show)

data RefreshFailure = RefreshFailure
  { failedRunId :: !Text
  , failedCompany :: !CompanyId
  , failedSource :: !SourceId
  , failedAt :: !UTCTime
  , failureReason :: !Text
  }
  deriving stock (Eq, Show)

data RefreshState = RefreshState
  { lastAttemptAt :: !UTCTime
  , lastSuccessfulAt :: !(Maybe UTCTime)
  , lastFailureReason :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

data FactPeriod = FactPeriod
  { factPeriodStart :: !Day
  , factPeriodEnd :: !Day
  , factPeriodType :: !PeriodType
  }
  deriving stock (Eq, Ord, Show)

data StoreError
  = DatabaseFailure !Text
  | InvalidPayloadDigest !Text !Text
  | PayloadTooLarge !Integer !Integer
  | InvalidStoredValue !Text
  | InvalidRefreshBatch !Text
  | UnsupportedSchemaVersion !Int
  | InvalidWatchlistSymbol !Text
  deriving stock (Eq, Show)

-- | Explicit caller choice for cache-first, refresh-first, and offline use.
data CacheMode = PreferCache | RefreshRequired | OfflineOnly
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data CacheStatus
  = CacheHit !UTCTime
  | CacheMiss
  | CacheStale !UTCTime
  deriving stock (Eq, Show)

{- | Apply a caller-selected freshness window. A future cache timestamp is
surfaced as stale so clock skew cannot make uncertain data look fresh.
-}
cacheStatusAt :: UTCTime -> NominalDiffTime -> Maybe UTCTime -> CacheStatus
cacheStatusAt _ _ Nothing = CacheMiss
cacheStatusAt now maxAge (Just lastSuccess)
  | age < 0 || age > maxAge = CacheStale lastSuccess
  | otherwise = CacheHit lastSuccess
  where
    age = diffUTCTime now lastSuccess

data CacheDecision
  = UseCachedData
  | UseStaleDataWithWarning
  | RefreshBeforeUse
  | RefreshThenAllowCachedFallback
  | OfflineDataUnavailable
  deriving stock (Eq, Show)

-- | Pure policy mapping for explicit online/cache/offline application modes.
decideCache :: CacheMode -> CacheStatus -> CacheDecision
decideCache PreferCache (CacheHit _) = UseCachedData
decideCache PreferCache (CacheStale _) = RefreshThenAllowCachedFallback
decideCache PreferCache CacheMiss = RefreshThenAllowCachedFallback
decideCache RefreshRequired _ = RefreshBeforeUse
decideCache OfflineOnly (CacheHit _) = UseCachedData
decideCache OfflineOnly (CacheStale _) = UseStaleDataWithWarning
decideCache OfflineOnly CacheMiss = OfflineDataUnavailable

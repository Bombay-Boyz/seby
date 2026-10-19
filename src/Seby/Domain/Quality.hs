{-# LANGUAGE DerivingStrategies #-}

{- | Quality classification and audit metadata for financial facts.
Enforces explicit distinction between authoritative filings,
secondary providers, and disputed figures.
-}
module Seby.Domain.Quality (
  QualityTier (..),
  QualityAudit (..),
  mkQualityAudit,
) where

import Data.Text (Text)

-- | Qualitative trustworthiness tier of a financial observation.
data QualityTier
  = AuthoritativePrimary -- Direct statutory filing (NSE/BSE/Annual Report)
  | VerifiedSecondary -- Secondary provider reconciled with primary filing
  | DisputedObservation !Text -- Conflicting figures between multiple sources
  | UnauditedEstimate !Text -- Forward or provisional estimate
  deriving stock (Eq, Show)

-- | Comprehensive audit record for an observation.
data QualityAudit = QualityAudit
  { auditTier :: !QualityTier
  , auditNotes :: !(Maybe Text)
  , auditVerified :: !Bool
  }
  deriving stock (Eq, Show)

mkQualityAudit :: QualityTier -> Maybe Text -> QualityAudit
mkQualityAudit tier notes =
  QualityAudit
    { auditTier = tier
    , auditNotes = notes
    , auditVerified = case tier of
        AuthoritativePrimary -> True
        VerifiedSecondary -> True
        _ -> False
    }

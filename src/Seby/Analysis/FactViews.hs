{-# LANGUAGE DerivingStrategies #-}

{- | Explicit temporal views over immutable canonical-fact revisions.
Reported chooses the first published version for each fact identity;
restated chooses the latest version; point-in-time excludes versions not
published by the requested instant.
-}
module Seby.Analysis.FactViews (
  FactView (..),
  selectFactView,
) where

import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime)
import Seby.Domain.Types (Provenance (..))
import Seby.Persistence.Types (CanonicalFact (..))

data FactView = AsReportedFacts | RestatedFacts | FactsKnownAt !UTCTime
  deriving stock (Eq, Show)

selectFactView :: FactView -> [CanonicalFact] -> [CanonicalFact]
selectFactView view facts = sortOn factKey (Map.elems selected)
  where
    eligible = filter (isEligible view . canonicalProvenance) facts
    selected = Map.fromListWith (choose view) [(factKey fact, fact) | fact <- eligible]

factKey :: CanonicalFact -> (String, String, String, String, String, String, String, String)
factKey fact =
  ( show (canonicalCompany fact)
  , show (canonicalConcept fact)
  , show (canonicalPeriodStart fact)
  , show (canonicalPeriodEnd fact)
  , show (canonicalStatementBasis fact)
  , show (canonicalScope fact)
  , show (canonicalCurrency fact)
  , show (canonicalPeriodType fact)
  )

isEligible :: FactView -> Provenance -> Bool
isEligible (FactsKnownAt instant) provenance = maybe False (<= instant) (provenancePublishedAt provenance)
isEligible _ _ = True

choose :: FactView -> CanonicalFact -> CanonicalFact -> CanonicalFact
choose view left right =
  let ordering = compare (rank left) (rank right)
   in case view of
        AsReportedFacts -> if ordering == GT then right else left
        _ -> if ordering == LT then right else left

rank :: CanonicalFact -> (UTCTime, UTCTime, String)
rank fact =
  ( fromMaybe (provenanceRetrievedAt provenance) (provenancePublishedAt provenance)
  , provenanceRetrievedAt provenance
  , show (provenanceDigest provenance)
  )
  where
    provenance = canonicalProvenance fact

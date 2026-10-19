{-# LANGUAGE DerivingStrategies #-}

{- | Effectful fact repository contract. Domain calculations depend only on
canonical facts and never receive a database connection.
-}
module Seby.Persistence.Repository (
  FactRepository (..),
) where

import Seby.Domain.Types (CompanyId)
import Seby.Persistence.Types

data FactRepository = FactRepository
  { closeFactRepository :: IO ()
  , storeRefreshBatch :: RefreshBatch -> IO (Either StoreError ())
  , recordRefreshFailure :: RefreshFailure -> IO (Either StoreError ())
  , loadCanonicalFacts :: CompanyId -> FactPeriod -> IO (Either StoreError [CanonicalFact])
  , getRefreshState :: CompanyId -> IO (Either StoreError (Maybe RefreshState))
  }

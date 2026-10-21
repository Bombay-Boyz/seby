{-# LANGUAGE DerivingStrategies #-}

{- | Effectful fact repository contract. Domain calculations depend only on
canonical facts and never receive a database connection.
-}
module Seby.Persistence.Repository (
  FactRepository (..),
) where

import Data.Text (Text)
import Seby.Analysis.CorporateActions (CorporateAction)
import Seby.Domain.Types (CompanyId, Symbol)
import Seby.Persistence.Types

data FactRepository = FactRepository
  { closeFactRepository :: IO ()
  , storeRefreshBatch :: RefreshBatch -> IO (Either StoreError ())
  , recordRefreshFailure :: RefreshFailure -> IO (Either StoreError ())
  , loadCanonicalFacts :: CompanyId -> FactPeriod -> IO (Either StoreError [CanonicalFact])
  , loadCanonicalFactHistory :: CompanyId -> FactPeriod -> IO (Either StoreError [CanonicalFact])
  , loadCurrentCompanyFacts :: CompanyId -> IO (Either StoreError [CanonicalFact])
  , loadCompanyName :: CompanyId -> IO (Either StoreError (Maybe Text))
  , persistCorporateActions :: [CorporateAction] -> IO (Either StoreError ())
  , loadCorporateActions :: CompanyId -> IO (Either StoreError [CorporateAction])
  , addWatchlistSymbol :: Symbol -> IO (Either StoreError ())
  , removeWatchlistSymbol :: Symbol -> IO (Either StoreError ())
  , loadWatchlist :: IO (Either StoreError [Symbol])
  , checkDatabaseIntegrity :: IO (Either StoreError ())
  , vacuumDatabase :: IO (Either StoreError ())
  , backupDatabase :: FilePath -> IO (Either StoreError ())
  , getRefreshState :: CompanyId -> IO (Either StoreError (Maybe RefreshState))
  }

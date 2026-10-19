{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Application orchestration and top-level command execution.
module Seby.App (
  Command (..),
  runCommand,
) where

import Data.Text.IO qualified as TIO
import Seby.Domain.Types (Symbol (..))

-- | Supported CLI commands for Seby.
data Command
  = AnalyseCompany !Symbol
  | PrintVersion
  deriving stock (Eq, Show)

-- | Execute a parsed CLI command.
runCommand :: Command -> IO ()
runCommand cmd = case cmd of
  AnalyseCompany (Symbol sym) -> do
    TIO.putStrLn $ "Seby: Initialising analysis for symbol " <> sym
    TIO.putStrLn "Target Market: NSE/BSE (Indian Listed Equities)"
    TIO.putStrLn "Phase 0 Toolchain: Pinned to GHC 9.14 LTS"
  PrintVersion -> do
    TIO.putStrLn "seby 0.1.0.0 — Indian Equities Fundamental Analysis System (Haskell GHC 9.14)"

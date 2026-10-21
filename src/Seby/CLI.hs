{-# LANGUAGE OverloadedStrings #-}

-- | Command-line interface parser for Seby using optparse-applicative.
module Seby.CLI (
  runCLI,
  commandParser,
) where

import Data.Char (toUpper)
import Data.Text qualified as T
import Options.Applicative
import Seby.App (Command (..), runCommand)
import Seby.Domain.Types (StatementBasis (..), Symbol (..))
import Seby.Persistence.Types (CacheMode (..))

-- | Main entry point for CLI execution.
runCLI :: IO ()
runCLI = execParser opts >>= runCommand
  where
    opts =
      info
        (commandParser <**> helper)
        ( fullDesc
            <> progDesc "Seby — Rigorous, local-first fundamental equity analysis for Indian markets"
            <> header "seby — CLI equity research for Indian listed companies"
        )

-- | Parser for top-level commands.
commandParser :: Parser Command
commandParser =
  subparser
    ( command
        "analyse"
        (info analyseParser (progDesc "Generate sourced fundamental reports for a ticker or the watchlist"))
        <> command
          "facts"
          ( info
              ( subparser
                  ( command
                      "import"
                      ( info
                          (ImportStatements <$> strArgument (metavar "FILE" <> help "Local sourced statement JSON"))
                          (progDesc "Import an analyst-prepared sourced statement file")
                      )
                      <> command
                        "refresh"
                        ( info
                            ( ( makeRefresh
                                  <$> symbolArgument
                                  <*> many
                                    ( option
                                        (eitherReader parseSource)
                                        ( long "source"
                                            <> metavar "PROVIDER:SYMBOL"
                                            <> help "Ordered source, repeat for fallback; e.g. fmp:TCS.NS or alpha-vantage:TCS.BSE"
                                        )
                                    )
                                  <*> option
                                    (eitherReader parseCacheMode)
                                    ( long "mode"
                                        <> value RefreshRequired
                                        <> metavar "MODE"
                                        <> help "prefer-cache, refresh, or offline (default: refresh)"
                                    )
                                  <*> option
                                    (eitherReader positiveHours)
                                    ( long "max-age-hours"
                                        <> value 24
                                        <> metavar "HOURS"
                                        <> help "Freshness window for --mode prefer-cache (default: 24)"
                                    )
                              )
                                <**> helper
                            )
                            (progDesc "Refresh financial statements or use locally cached facts")
                        )
                  )
              )
              (progDesc "Import local statement facts")
          )
        <> command
          "watchlist"
          ( info
              ( subparser
                  ( command
                      "add"
                      (info (WatchlistAdd <$> symbolArgument) (progDesc "Add a ticker to the local watchlist"))
                      <> command
                        "remove"
                        (info (WatchlistRemove <$> symbolArgument) (progDesc "Remove a ticker from the local watchlist"))
                      <> command "list" (info (pure WatchlistList) (progDesc "List local watchlist tickers"))
                  )
              )
              (progDesc "Manage a local ticker watchlist")
          )
        <> command
          "db"
          ( info
              ( subparser
                  ( command "check" (info (pure DatabaseCheck) (progDesc "Run SQLite integrity check"))
                      <> command "vacuum" (info (pure DatabaseVacuum) (progDesc "Reclaim unused SQLite space"))
                      <> command
                        "backup"
                        ( info
                            (DatabaseBackup <$> strArgument (metavar "FILE" <> help "New backup file path"))
                            (progDesc "Create a consistent SQLite snapshot, including WAL data")
                        )
                  )
              )
              (progDesc "Maintain the local database")
          )
    )
    <|> flag'
      PrintVersion
      (long "version" <> short 'v' <> help "Print Seby version and exit")
  where
    symbolArgument = Symbol . T.toUpper . T.pack <$> strArgument (metavar "SYMBOL" <> help "Listed-company ticker")
    analyseParser =
      makeAnalyse
        <$> optional
          (Symbol . T.toUpper . T.pack <$> strArgument (metavar "SYMBOL" <> help "Listed-company ticker"))
        <*> switch (long "watchlist" <> help "Analyse every ticker in the local watchlist")
        <*> option
          (eitherReader parseBasis)
          ( long "basis"
              <> value Consolidated
              <> metavar "BASIS"
              <> help "consolidated or standalone (default: consolidated)"
          )
        <*> strOption
          (long "scope" <> value "group" <> metavar "SCOPE" <> help "Consolidation scope (default: group)")
    makeAnalyse maybeSymbol allWatched basis scope = case (maybeSymbol, allWatched) of
      (Just symbol, False) -> AnalyseCompany symbol basis scope
      (Nothing, True) -> AnalyseWatchlist basis scope
      (Just _, True) -> InvalidAnalysisRequest "provide SYMBOL or --watchlist, not both"
      (Nothing, False) -> InvalidAnalysisRequest "provide SYMBOL or --watchlist"
    parseBasis basisInput = case map toUpper basisInput of
      "CONSOLIDATED" -> Right Consolidated
      "STANDALONE" -> Right Standalone
      _ -> Left "basis must be consolidated or standalone"
    parseSource source = case break (== ':') source of
      (providerName, ':' : providerSymbol)
        | map toUpper providerName `elem` ["FMP", "ALPHA-VANTAGE"] && not (null providerSymbol) ->
            Right (T.pack providerName, Symbol (T.toUpper (T.pack providerSymbol)))
      _ -> Left "source must be PROVIDER:SYMBOL, with provider fmp or alpha-vantage"
    parseCacheMode mode = case map toUpper mode of
      "PREFER-CACHE" -> Right PreferCache
      "REFRESH" -> Right RefreshRequired
      "OFFLINE" -> Right OfflineOnly
      _ -> Left "mode must be prefer-cache, refresh, or offline"
    positiveHours raw = case reads raw :: [(Int, String)] of
      [(hours, "")] | hours > 0 -> Right hours
      _ -> Left "max-age-hours must be a positive integer"
    makeRefresh symbol sources mode hours
      | mode /= OfflineOnly && null sources =
          InvalidAnalysisRequest "provide at least one --source unless --mode offline is selected"
      | otherwise = RefreshStatements symbol sources mode (fromIntegral hours * 3600)

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
                            ( RefreshStatements
                                <$> symbolArgument
                                <*> some
                                  ( option
                                      (eitherReader parseSource)
                                      ( long "source"
                                          <> metavar "PROVIDER:SYMBOL"
                                          <> help "Ordered source, repeat for fallback; e.g. fmp:TCS.NS or alpha-vantage:TCS.BSE"
                                      )
                                  )
                            )
                            (progDesc "Refresh financial statements from an explicitly selected provider")
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

{-# LANGUAGE OverloadedStrings #-}

-- | Command-line interface parser for Seby using optparse-applicative.
module Seby.CLI (
  runCLI,
  commandParser,
) where

import Data.Text qualified as T
import Options.Applicative
import Seby.App (Command (..), runCommand)
import Seby.Domain.Types (Symbol (..))

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
        ( info
            ( AnalyseCompany . Symbol . T.pack
                <$> strArgument
                  (metavar "SYMBOL" <> help "NSE/BSE listed company ticker (e.g. TCS, INFY, RELIANCE)")
            )
            (progDesc "Analyse a listed company and generate research report")
        )
    )
    <|> flag'
      PrintVersion
      (long "version" <> short 'v' <> help "Print Seby version and exit")

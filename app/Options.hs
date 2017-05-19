{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, RecordWildCards #-}
module Options
    (
      CommandLine(..)
    , commandLine
    , parseCommandLine
    , versionInfo
    ) where

import Data.Monoid ((<>))
import Data.Version (showVersion)
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Paths_criterion (version)
import Options.Applicative

data CommandLine
    = Report { jsonFile :: FilePath, outputFile :: FilePath, templateFile :: FilePath }
    | Version
    deriving (Eq, Read, Show, Typeable, Data, Generic)

reportOptions :: Parser CommandLine
reportOptions = Report <$> measurements <*> outputFile <*> templateFile
  where
    measurements = strArgument $ mconcat
        [metavar "INPUT-JSON", help "Json file to read Criterion output from."]

    outputFile = strArgument $ mconcat
        [metavar "OUTPUT-FILE", help "File to output formatted report too."]

    templateFile = strOption $ mconcat
        [ long "template", short 't', metavar "FILE", value "default",
          help "Template to use for report." ]

parseCommand :: Parser CommandLine
parseCommand =
  (Version <$ switch (long "version" <> help "Show version info")) <|>
  (subparser $
    command "report" (info analyseOptions (progDesc "Generate report.")))

commandLine :: ParserInfo CommandLine
commandLine = info (helper <*> parseCommand) $
  header versionInfo <>
  fullDesc

parseCommandLine :: IO CommandLine
parseCommandLine = execParser commandLine

versionInfo :: String
versionInfo = "criterion " ++ showVersion version

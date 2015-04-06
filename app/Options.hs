{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, RecordWildCards #-}

module Options
    (
      CommandLine(..)
    , commandLine
    , parseCommandLine
    , versionInfo
    ) where

import Data.Version (showVersion)
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Paths_criterion (version)
import Options.Applicative

data CommandLine = Analyse [FilePath]
                 | Version
                 deriving (Eq, Read, Show, Typeable, Data, Generic)

analyseOptions :: Parser CommandLine
analyseOptions = Analyse <$> some (argument str (metavar "FILE [...]"))

parseCommand :: Parser CommandLine
parseCommand =
  (Version <$ switch (long "version" <> help "Show version info")) <|>
  (subparser $
    command "analyse" (info analyseOptions (progDesc "Analyse measurements")))

commandLine :: ParserInfo CommandLine
commandLine = info (helper <*> parseCommand) $
  header versionInfo <>
  fullDesc

parseCommandLine :: IO CommandLine
parseCommandLine = execParser commandLine

versionInfo :: String
versionInfo = "criterion " ++ showVersion version

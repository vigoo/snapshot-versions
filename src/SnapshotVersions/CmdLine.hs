module SnapshotVersions.CmdLine where

import           Control.Applicative
import           Options.Applicative

type SnapshotName = String
data OutputType = Default
                | StackYaml
                | CabalConstraints

data Parameters
  = Parameters
    { pCabal    :: FilePath
    , pSnapshot :: SnapshotName
    , pOutput   :: OutputType
    , pDebug    :: Bool
    }

parameters :: Parser Parameters
parameters = Parameters
         <$> strArgument (metavar "CABAL" <> help "Cabal file to be analyzed")
         <*> strArgument (metavar "NAME" <> help "Stackage snapshot name to get the versions from")
         <*> (flag' StackYaml (long "stack-yaml") <|> flag' CabalConstraints (long "cabal-constraints") <|> pure Default)
         <*> switch (long "debug" <> help "Turns on debug output")

withParameters :: (Parameters -> IO a) -> IO a
withParameters fn = execParser opts >>= fn
  where
    opts = info (helper <*> parameters)
             ( fullDesc
            <> progDesc "Gets the versions of build dependencies from a given stackage snapshot"
            <> header "snapshot-versions"
             )

module SnapshotVersions.CmdLine where

import           Control.Applicative
import           Options.Applicative
import           SnapshotVersions.Snapshot

data OutputType = Default
                | StackYaml
                | CabalConstraints

data Parameters
  = Parameters
    { pCabal    :: FilePath
    , pSnapshot :: SnapshotName
    , pOutput   :: OutputType
    }

parameters :: Parser Parameters
parameters = Parameters
         <$> strArgument (metavar "CABAL" <> help "Cabal file to be analyzed")
         <*> strArgument (metavar "NAME" <> help "Stackage snapshot name to get the versions from")
         <*> (flag' StackYaml (long "stack-yaml") <|> flag' CabalConstraints (long "cabal-constraints") <|> pure Default)

withParameters :: (Parameters -> IO a) -> IO a
withParameters fn = execParser opts >>= fn
  where
    opts = info (helper <*> parameters)
             ( fullDesc
            <> progDesc "Gets the versions of build dependencies from a given stackage snapshot"
            <> header "snapshot-versions"
             )

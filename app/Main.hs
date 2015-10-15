{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad
import           Control.Monad.Reader
import qualified Data.Map                           as Map
import           Data.Monoid
import qualified Data.Set                           as Set
import           SnapshotVersions.Cabal
import           SnapshotVersions.CmdLine
import           SnapshotVersions.Output
import           SnapshotVersions.PackageIndex
import           SnapshotVersions.ProcessedPackages
import           SnapshotVersions.Snapshot

main :: IO ()
main =
  withParameters $ \(Parameters{..}) ->
    withOutput pOutput pDebug $ do
      info $ "Fetching snapshot " <> pSnapshot <> "..."
      withVersionMap pSnapshot $ do
        info $ "Fetched."
        info $ "Initializing package index"
        indexReader <- createIndexReader
        info $ "Getting dependent libraries from " <> pCabal <> "..."
        deps <- findAllDependencies (Left pCabal) indexReader (asProcessedPackages Set.empty)

        versionMap <- getVersionMap
        resultStart
        forM_ (toList deps) $ \(DependentPackage pkg explicitVer) ->
          if (Map.member pkg (asMap versionMap))
          then case ((asMap versionMap) Map.! pkg) of
              ExplicitVersion ver -> result pkg ver
              _ -> return ()
          else case explicitVer  of
             Nothing -> logWarning $ show pkg <> " is not part of the snapshot; specifying its version in .cabal is recommended"
             _ -> return ()
        resultEnd

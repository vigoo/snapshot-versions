{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad
import qualified Data.Map                      as Map
import           Data.Monoid
import qualified Data.Set                      as Set
import           SnapshotVersions.Cabal
import           SnapshotVersions.CmdLine
import           SnapshotVersions.Output
import           SnapshotVersions.PackageIndex
import           SnapshotVersions.Snapshot

main :: IO ()
main = withParameters $ \(Parameters{..}) -> do
  debug pOutput $ "Fetching snapshot " <> pSnapshot <> "..."
  versionMap' <- fetchVersionMap pSnapshot
  case versionMap' of
    Nothing -> logError pOutput "Failed to fetch snapshot."
    Just versionMap -> do
      debug pOutput $ "Fetched."
      debug pOutput $ "Initializing package index"
      indexReader <- createIndexReader pOutput
      debug pOutput  $ "Getting dependent libraries from " <> pCabal <> "..."
      deps <- findAllDependencies pOutput (Left pCabal) versionMap indexReader Set.empty

      resultStart pOutput
      forM_ deps $ \pkg ->
        when (Map.member pkg (asMap versionMap)) $
          result pOutput pkg ((asMap versionMap) Map.! pkg)
      resultEnd pOutput

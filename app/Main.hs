{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad
import qualified Data.ByteString.Char8              as B8
import qualified Data.Map                           as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                           as Set
import           SnapshotVersions.Cabal
import           SnapshotVersions.CmdLine
import           SnapshotVersions.Output
import           SnapshotVersions.PackageIndex
import           SnapshotVersions.ProcessedPackages
import           SnapshotVersions.Snapshot

dumpResults :: ProcessedPackages -> VersionMapReader (OutputMonad IO) ()
dumpResults deps = getVersionMap >>= \versionMap -> do
  results <- forM (toList deps) $ \(DependentPackage pkg explicitVer) ->
    if Map.member (B8.pack pkg) (asMap versionMap)
    then case (asMap versionMap) Map.! (B8.pack pkg) of
      ExplicitVersion ver -> return $ Just (pkg, B8.unpack ver)
      _ -> return Nothing
    else case explicitVer  of
      Nothing -> do
        logWarning $ show pkg <> " is not part of the snapshot; specifying its version in .cabal is recommended"
        return Nothing
      Just ver -> return $ Just (pkg, ver)

  let filteredResults = catMaybes results

  resultStart
  forM_ (filteredResults `zip` [0..]) $ \((pkg, ver), i) -> result (i `toListPos` (length filteredResults)) pkg ver
  resultEnd

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
        deps <- findAllDependencies (Left pCabal) indexReader (asProcessedPackages Set.empty) True

        dumpResults deps

{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad
import qualified Data.Map                  as Map
import           Data.Monoid
import qualified Data.Set                  as Set
import           SnapshotVersions.Cabal
import           SnapshotVersions.CmdLine
import           SnapshotVersions.Snapshot
import           System.IO

main :: IO ()
main = withParameters $ \(Parameters{..}) -> do
  putStrLn $ "Fetching snapshot " <> pSnapshot <> "..."
  versionMap' <- fetchVersionMap pSnapshot
  case versionMap' of
    Nothing -> hPutStrLn stderr "Failed to fetch snapshot."
    Just versionMap -> do
      putStrLn "Fetched."
      putStrLn $ "Getting dependent libraries from " <> pCabal <> "..."
      deps <- findAllDependencies pCabal versionMap Set.empty

      putStrLn "Results:"
      forM_ deps $ \pkg ->
        when (Map.member pkg (asMap versionMap)) $
          putStrLn $ pkg <> "-" <> ((asMap versionMap) Map.! pkg)

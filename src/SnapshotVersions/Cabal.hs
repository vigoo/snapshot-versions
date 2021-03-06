{-# LANGUAGE RecordWildCards #-}
module SnapshotVersions.Cabal
       ( findAllDependencies
       )
       where

import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8                 as B8
import qualified Data.ByteString.Lazy.Char8            as BL
import qualified Data.Map                              as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                              as Set
import           Data.Version
import qualified Distribution.Package                  as Pkg
import           Distribution.PackageDescription.Parse
import           Distribution.Verbosity
import           Distribution.Version
import           Network.HTTP.Conduit
import           SnapshotVersions.Output
import           SnapshotVersions.PackageIndex
import           SnapshotVersions.ProcessedPackages
import           SnapshotVersions.Snapshot

findAllDependencies :: (Monad m, MonadIO m, MonadOutput m, MonadVersionMap m) => Either FilePath LightweightPackageDescription -> IndexReader m -> ProcessedPackages -> Bool -> m ProcessedPackages
findAllDependencies (Left p) indexReader processed toplevel = do
  pkgDesc <- liftIO $ readPackageDescription silent p
  findAllDependencies (Right (toLightweightPackageDescription pkgDesc)) indexReader processed toplevel
findAllDependencies (Right pkgDesc) indexReader processed toplevel =
  case pkgDesc of
    MissingDependencies missingDeps -> do
      logError $ "Could not finalize package description, missing dependencies: " <> show missingDeps
      return $ asProcessedPackages Set.empty
    LightweightPackageDescription{..} -> do
      let pkg = Pkg.unPackageName lpdName
      let pkgVer = Just $ showVersion lpdVersion
      debug $ "Processing " <> pkg <> "-" <> fromJust pkgVer
      let processed' =
            if toplevel
            then asProcessedPackages Set.empty
            else addProcessedPackage (DependentPackage pkg pkgVer) processed
      let newPkgs = findNewPackages processed' $ Set.fromList $ map packageFromDependency lpdDependencies
      childDeps <- indented $ recursiveFindDeps indexReader (toList newPkgs) processed'
      return $ mergeProcessedPackages [processed, newPkgs, childDeps]

recursiveFindDeps :: (Monad m, MonadIO m, MonadOutput m, MonadVersionMap m) => IndexReader m -> [DependentPackage] -> ProcessedPackages -> m ProcessedPackages
recursiveFindDeps indexReader (pkg:pkgs) processed =
  if isProcessed pkg processed && not (addsExplicitVersion pkg processed)
  then do
    debug $ pkgName <> " is already processed"
    recursiveFindDeps indexReader pkgs processed
  else do
    versionMap <- getVersionMap
    if Map.member pkgNameB8 (asMap versionMap)
    then do
      -- The package is part of the snapshot. We use the snapshot version to read its package
      -- definition and traverse its dependencies
      debug $ pkgName <> " is specified in the snapshot"
      proceedWithPackageVersion (asMap versionMap Map.! pkgNameB8)
    else do
      -- The package is not part of the snapshot. If there is an explicit version constraint,
      -- we can traverse its dependencies, otherwise we just add it to the result set
      -- (which doesn't really do anything because in the final step we'll look up the result packages
      -- in the snapshot version map)
      case dpExplicitVersion pkg of
        Just pkgVer -> do
          debug $ pkgName <> " has explicit version " <> pkgVer
          proceedWithPackageVersion (ExplicitVersion (B8.pack pkgVer))
        Nothing -> do
          debug $ pkgName <> " has no explicit version"
          recursiveFindDeps indexReader pkgs (addProcessedPackage pkg processed)

  where
    pkgName = dpName pkg
    pkgNameB8 = B8.pack pkgName
    proceedWithPackageVersion (ExplicitVersion ver) = do
      debug $ "Reading cabal for " <> pkgName <> " version " <> B8.unpack ver
      pkgDesc <- fetchCabal indexReader pkgName (B8.unpack ver)
      childDeps <- findAllDependencies (Right pkgDesc) indexReader processed False
      let newProcessed = addProcessedPackage (DependentPackage pkgName (Just (B8.unpack ver))) $ mergeProcessedPackages [processed, childDeps]
      recursiveFindDeps indexReader pkgs newProcessed
    proceedWithPackageVersion InstalledGlobal =
      recursiveFindDeps indexReader pkgs (addProcessedPackage pkg processed)

recursiveFindDeps _ [] processed = do
  debug $ "---"
  return processed

packageFromDependency :: Pkg.Dependency -> DependentPackage
packageFromDependency (Pkg.Dependency name versionRange) =
  DependentPackage { dpName = Pkg.unPackageName name
                   , dpExplicitVersion = explicitVersionFromRange versionRange
                   }

explicitVersionFromRange :: VersionRange -> Maybe String
explicitVersionFromRange vr = showVersion <$> isSpecificVersion vr

fetchCabal :: (Monad m, MonadIO m, MonadOutput m) => (IndexReader m) -> String -> String -> m LightweightPackageDescription
fetchCabal reader name ver = do
  pkgDesc <- reader name ver
  case pkgDesc of
    Just pkgDesc' -> return pkgDesc'
    Nothing -> fallback

  where
    fallback = do
      let url = "https://raw.githubusercontent.com/commercialhaskell/all-cabal-hashes/hackage/" <> name <> "/" <> ver <> "/" <> name <> ".cabal"
      debug $ "Pulling " <> url
      body <- simpleHttp url
      return $ fromJust $ tryParsePackageDescription $ BL.unpack body -- TODO: error handling

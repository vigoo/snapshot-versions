{-# LANGUAGE RecordWildCards #-}

module SnapshotVersions.Cabal where

import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8                    as BL
import qualified Data.Map                                      as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                                      as Set
import           Data.Version
import           Distribution.Compiler
import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Configuration
import           Distribution.PackageDescription.Parse
import           Distribution.System
import           Distribution.Verbosity
import           Distribution.Version
import           Network.HTTP.Conduit
import           SnapshotVersions.CmdLine
import           SnapshotVersions.Output
import           SnapshotVersions.PackageIndex
import           SnapshotVersions.ProcessedPackages
import           SnapshotVersions.Snapshot

findAllDependencies :: (Monad m, MonadIO m, MonadOutput m, MonadVersionMap m) => (Either FilePath GenericPackageDescription) -> (IndexReader m) -> ProcessedPackages -> m (ProcessedPackages)
findAllDependencies (Left path) indexReader processed = do
  pkgDesc <- liftIO $ readPackageDescription silent path
  findAllDependencies (Right pkgDesc) indexReader processed
findAllDependencies (Right pkgDesc) indexReader processed = do
  let finalizeResult = finalizePackageDescription
                         []             -- TODO: configurable flags
                         (const True)
                         buildPlatform  -- TODO: configurable platform
                         (unknownCompilerInfo
                          (CompilerId buildCompilerFlavor (Version [] [])) NoAbiTag)
                         []
                         pkgDesc
  case finalizeResult of
    Left missingDeps -> do
      logError $ "Could not finalize package description, missing dependencies: " <> show missingDeps
      return $ asProcessedPackages Set.empty
    Right (finalizedPkgDesc, _) -> do
      let pkg = unPackageName (pkgName (package finalizedPkgDesc))
      let pkgVer = Just $ showVersion (pkgVersion (package finalizedPkgDesc))
      debug $ "Processing " <> pkg <> "-" <> fromJust pkgVer
      let processed' = addProcessedPackage (DependentPackage pkg pkgVer) processed
      let newPkgs = findNewPackages processed' $ Set.fromList $ map packageFromDependency (buildDepends finalizedPkgDesc)
      childDeps <- indented $ recursiveFindDeps indexReader (toList newPkgs) processed'
      return $ mergeProcessedPackages [processed, newPkgs, childDeps]

recursiveFindDeps :: (Monad m, MonadIO m, MonadOutput m, MonadVersionMap m) => (IndexReader m) -> [DependentPackage] -> ProcessedPackages -> m (ProcessedPackages)
recursiveFindDeps indexReader (pkg:pkgs) processed =
  if (isProcessed pkg processed && not (addsExplicitVersion pkg processed))
  then do
    debug $ pkgName <> " is already processed"
    recursiveFindDeps indexReader pkgs processed
  else do
    versionMap <- getVersionMap
    if (Map.member pkgName (asMap versionMap))
    then do
      -- The package is part of the snapshot. We use the snapshot version to read its package
      -- definition and traverse its dependencies
      debug $ pkgName <> " is specified in the snapshot"
      proceedWithPackageVersion ((asMap versionMap) Map.! pkgName)
    else do
      -- The package is not part of the snapshot. If there is an explicit version constraint,
      -- we can traverse its dependencies, otherwise we just add it to the result set
      -- (which doesn't really do anything because in the final step we'll look up the result packages
      -- in the snapshot version map)
      case (dpExplicitVersion pkg) of
        Just pkgVer -> do
          debug $ pkgName <> " has explicit version " <> pkgVer
          proceedWithPackageVersion (ExplicitVersion pkgVer)
        Nothing -> do
          debug $ pkgName <> " has no explicit version"
          recursiveFindDeps indexReader pkgs (addProcessedPackage pkg processed)

  where
    pkgName = dpName pkg
    proceedWithPackageVersion (ExplicitVersion ver) = do
      debug $ "Reading cabal for " <> pkgName <> " version " <> ver
      pkgDesc <- fetchCabal indexReader pkgName ver
      childDeps <- findAllDependencies (Right pkgDesc) indexReader processed
      let newProcessed = addProcessedPackage (DependentPackage pkgName (Just ver)) $ mergeProcessedPackages [processed, childDeps]
      recursiveFindDeps indexReader pkgs newProcessed
    proceedWithPackageVersion InstalledGlobal =
      recursiveFindDeps indexReader pkgs (addProcessedPackage pkg processed)

recursiveFindDeps _ [] processed = do
  debug $ "---"
  return processed

findLibraryDeps :: GenericPackageDescription -> Set.Set DependentPackage
findLibraryDeps (GenericPackageDescription{..}) =
  case condLibrary of
    Just (CondNode (Library{..}) _ _) -> pkgSet libBuildInfo
    Nothing -> Set.empty

pkgSet :: BuildInfo -> Set.Set DependentPackage
pkgSet bi = Set.fromList $ map packageFromDependency (targetBuildDepends bi)

findExecutableSuiteDeps :: GenericPackageDescription -> Set.Set DependentPackage
findExecutableSuiteDeps (GenericPackageDescription{..}) =
  foldl (\set d -> let (_, CondNode e _ _) = d in set `Set.union` (pkgSet (buildInfo e))) Set.empty condExecutables

findTestSuiteDeps :: GenericPackageDescription -> Set.Set DependentPackage
findTestSuiteDeps (GenericPackageDescription{..}) =
  foldl (\set d -> let (_, CondNode t _ _) = d in set `Set.union` (pkgSet (testBuildInfo t))) Set.empty condTestSuites

packageFromDependency :: Dependency -> DependentPackage
packageFromDependency (Dependency name versionRange) =
  DependentPackage { dpName = unPackageName name
                   , dpExplicitVersion = explicitVersionFromRange versionRange
                   }

explicitVersionFromRange :: VersionRange -> Maybe String
explicitVersionFromRange vr = showVersion <$> isSpecificVersion vr

fetchCabal :: (Monad m, MonadIO m, MonadOutput m) => (IndexReader m) -> String -> String -> m GenericPackageDescription
fetchCabal reader name ver = do
  pkgDesc <- reader name ver
  case pkgDesc of
    Just pkgDesc' -> return pkgDesc'
    Nothing -> fallback

  where
    fallback = do
      let url = "https://raw.githubusercontent.com/commercialhaskell/all-cabal-hashes/hackage/" <> name <> "/" <> ver <> "/" <> name <> ".cabal"
          path = "/tmp/" <> name <> "." <> ver <> ".cabal"
      debug $ "Pulling " <> url
      body <- simpleHttp url
      return $ fromJust $ tryParsePackageDescription $ BL.unpack body -- TODO: error handling

{-# LANGUAGE RecordWildCards #-}

module SnapshotVersions.Cabal where

import qualified Data.Set                              as Set
import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import           Distribution.Verbosity

findAllDependencies :: FilePath -> IO (Set.Set String)
findAllDependencies path = do
  pkgDesc <- readPackageDescription silent path
  return $ Set.unions [findLibraryDeps pkgDesc, findExecutableSuiteDeps pkgDesc, findTestSuiteDeps pkgDesc]

findLibraryDeps :: GenericPackageDescription -> Set.Set String
findLibraryDeps (GenericPackageDescription{..}) =
  case condLibrary of
    Just (CondNode (Library{..}) _ _) -> pkgSet libBuildInfo
    Nothing -> Set.empty

pkgSet :: BuildInfo -> Set.Set String
pkgSet bi = Set.fromList $ map packageFromDependency (targetBuildDepends bi)

findExecutableSuiteDeps :: GenericPackageDescription -> Set.Set String
findExecutableSuiteDeps (GenericPackageDescription{..}) =
  foldl (\set d -> let (_, CondNode e _ _) = d in set `Set.union` (pkgSet (buildInfo e))) Set.empty condExecutables

findTestSuiteDeps :: GenericPackageDescription -> Set.Set String
findTestSuiteDeps (GenericPackageDescription{..}) =
  foldl (\set d -> let (_, CondNode t _ _) = d in set `Set.union` (pkgSet (testBuildInfo t))) Set.empty condTestSuites

packageFromDependency :: Dependency -> String
packageFromDependency (Dependency name _) = unPackageName name

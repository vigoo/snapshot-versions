{-# LANGUAGE RecordWildCards #-}

module SnapshotVersions.Cabal where

import qualified Data.ByteString.Lazy.Char8            as BL
import qualified Data.Map                              as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                              as Set
import           Data.Version
import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import           Distribution.Verbosity
import           Distribution.Version
import           Network.HTTP.Conduit
import           SnapshotVersions.CmdLine
import           SnapshotVersions.Output
import           SnapshotVersions.PackageIndex
import           SnapshotVersions.Snapshot

data DependentPackage =
  DependentPackage { dpName            :: String
                   , dpExplicitVersion :: Maybe String
                   }

instance Show DependentPackage where
  show (DependentPackage dpName (Just ver)) = dpName <> "-" <> ver
  show (DependentPackage dpName Nothing) = dpName

instance Eq DependentPackage where
  a == b = (dpName a) == (dpName b)

instance Ord DependentPackage where
  a `compare` b = (dpName a) `compare` (dpName b)

findAllDependencies :: OutputType -> (Either FilePath GenericPackageDescription) -> VersionMap -> IndexReader -> Set.Set DependentPackage -> IO (Set.Set DependentPackage)
findAllDependencies outputType (Left path) versionMap indexReader processed = do
  pkgDesc <- readPackageDescription silent path
  findAllDependencies outputType (Right pkgDesc) versionMap indexReader processed
findAllDependencies outputType (Right pkgDesc) versionMap indexReader processed = do
  let pkg = unPackageName (pkgName (package (packageDescription pkgDesc)))
  let pkgVer = Just $ showVersion (pkgVersion (package (packageDescription pkgDesc)))
  let processed' = Set.union processed (Set.singleton (DependentPackage pkg pkgVer))
  let newPkgs = Set.difference (Set.unions [findLibraryDeps pkgDesc, findExecutableSuiteDeps pkgDesc, findTestSuiteDeps pkgDesc]) processed'
  childDeps <- recursiveFindDeps outputType versionMap indexReader (Set.toList newPkgs) processed'
  return $ Set.unions [processed, newPkgs, childDeps]

recursiveFindDeps :: OutputType -> VersionMap -> IndexReader -> [DependentPackage] -> Set.Set DependentPackage -> IO (Set.Set DependentPackage)
recursiveFindDeps outputType versionMap indexReader (pkg:pkgs) processed =
  if (Set.member pkg processed)
  then recursiveFindDeps outputType versionMap indexReader pkgs processed
  else do
    if (Map.member pkgName (asMap versionMap))
    then do
      -- The package is part of the snapshot. We use the snapshot version to read its package
      -- definition and traverse its dependencies
      proceedWithPackageVersion ((asMap versionMap) Map.! pkgName)
    else do
      -- The package is not part of the snapshot. If there is an explicit version constraint,
      -- we can traverse its dependencies, otherwise we just add it to the result set
      -- (which doesn't really do anything because in the final step we'll look up the result packages
      -- in the snapshot version map)
      case (dpExplicitVersion pkg) of
        Just pkgVer -> proceedWithPackageVersion (ExplicitVersion pkgVer)
        Nothing -> do
          logWarning outputType $ show pkg <> " is not part of the snapshot; specifying its version in .cabal is recommended"
          recursiveFindDeps outputType versionMap indexReader pkgs (Set.union processed (Set.singleton pkg))

  where
    pkgName = dpName pkg
    proceedWithPackageVersion (ExplicitVersion ver) = do
      debug outputType $ "Reading cabal for " <> pkgName <> " version " <> ver
      pkgDesc <- fetchCabal outputType indexReader pkgName ver
      childDeps <- findAllDependencies outputType (Right pkgDesc) versionMap indexReader processed
      let newProcessed = Set.unions [processed, Set.singleton pkg, childDeps]
      recursiveFindDeps outputType versionMap indexReader pkgs newProcessed
    proceedWithPackageVersion InstalledGlobal =
      recursiveFindDeps outputType versionMap indexReader pkgs (Set.union processed (Set.singleton pkg))

recursiveFindDeps _ versionMap _ [] processed = return processed

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

fetchCabal :: OutputType -> IndexReader -> String -> String -> IO GenericPackageDescription
fetchCabal outputType reader name ver = do
  pkgDesc <- reader name ver
  case pkgDesc of
    Just pkgDesc' -> return pkgDesc'
    Nothing -> fallback

  where
    fallback = do
      let url = "https://raw.githubusercontent.com/commercialhaskell/all-cabal-hashes/hackage/" <> name <> "/" <> ver <> "/" <> name <> ".cabal"
          path = "/tmp/" <> name <> "." <> ver <> ".cabal"
      debug outputType $ "Pulling " <> url
      body <- simpleHttp url
      return $ fromJust $ tryParsePackageDescription $ BL.unpack body -- TODO: error handling

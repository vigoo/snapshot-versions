{-# LANGUAGE RecordWildCards #-}

module SnapshotVersions.Cabal where

import qualified Data.ByteString.Lazy.Char8            as BL
import qualified Data.Map                              as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                              as Set
import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import           Distribution.Verbosity
import           Network.HTTP.Conduit
import           SnapshotVersions.CmdLine
import           SnapshotVersions.Output
import           SnapshotVersions.PackageIndex
import           SnapshotVersions.Snapshot

findAllDependencies :: OutputType -> (Either FilePath GenericPackageDescription) -> VersionMap -> IndexReader -> Set.Set String -> IO (Set.Set String)
findAllDependencies outputType (Left path) versionMap indexReader processed = do
  pkgDesc <- readPackageDescription silent path
  findAllDependencies outputType (Right pkgDesc) versionMap indexReader processed
findAllDependencies outputType (Right pkgDesc) versionMap indexReader processed = do
  let pkg = unPackageName (pkgName (package (packageDescription pkgDesc)))
  let processed' = Set.union processed (Set.singleton pkg)
  let newPkgs = Set.difference (Set.unions [findLibraryDeps pkgDesc, findExecutableSuiteDeps pkgDesc, findTestSuiteDeps pkgDesc]) processed'
  childDeps <- recursiveFindDeps outputType versionMap indexReader (Set.toList newPkgs) processed'
  return $ Set.unions [processed, newPkgs, childDeps]

recursiveFindDeps :: OutputType -> VersionMap -> IndexReader -> [String] -> Set.Set String -> IO (Set.Set String)
recursiveFindDeps outputType versionMap indexReader (pkg:pkgs) processed =
  if (Set.member pkg processed)
  then recursiveFindDeps outputType versionMap indexReader pkgs processed
  else do
    if (Map.member pkg (asMap versionMap))
    then do
      pkgDesc <- fetchCabal outputType indexReader pkg ((asMap versionMap) Map.! pkg)
      childDeps <- findAllDependencies outputType (Right pkgDesc) versionMap indexReader processed
      let newProcessed = Set.unions [processed, Set.singleton pkg, childDeps]
      recursiveFindDeps outputType versionMap indexReader pkgs newProcessed
    else
      recursiveFindDeps outputType versionMap indexReader pkgs (Set.union processed (Set.singleton pkg))

recursiveFindDeps _ versionMap _ [] processed = return processed

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

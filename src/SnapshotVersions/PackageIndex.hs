{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SnapshotVersions.PackageIndex where

import qualified Codec.Archive.Tar                             as Tar
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8                         as B
import qualified Data.ByteString.Lazy                          as BL
import           Data.IORef
import qualified Data.Map                                      as Map
import           Data.Monoid
import           Data.Serialize
import           Distribution.Compiler
import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Configuration
import           Distribution.PackageDescription.Parse
import           Distribution.System
import           Distribution.Version
import           GHC.Generics
import           SnapshotVersions.Cache
import           SnapshotVersions.Output
import           System.Directory
import           System.Environment
import           System.FilePath

data LightweightPackageDescription
  = LightweightPackageDescription
    { lpdName         :: PackageName
    , lpdVersion      :: Version
    , lpdDependencies :: [Dependency]
    }
  | MissingDependencies
    { lpdMissingDeps :: [Dependency]
    }
  deriving (Show, Generic)

instance Serialize LightweightPackageDescription

type IndexReader m = String -> String -> m (Maybe LightweightPackageDescription)

instance Serialize PackageName
instance Serialize VersionRange
instance Serialize Dependency
instance Serialize Version

indexPath :: IO FilePath
indexPath = do
    env <- getEnvironment
    stackRoot <- case lookup "STACK_ROOT" env of
                    Nothing -> do
                        home <- getHomeDirectory
                        return (home </> ".stack")
                    Just path ->
                        return path
    return (stackRoot </> "indices" </> "Hackage" </> "00-index.tar")

createIndexReader :: (Monad m, MonadIO m, MonadOutput m) => m (IndexReader m)
createIndexReader = do
  path <- liftIO indexPath
  contentReader <- createIndexReaderFor path
  return $ \name version -> do
    let relPath = name </> version </> (name <> ".cabal")
    cachedMaybe ("pkgdesc." <> name <> "." <> version) $ do
      raw <- contentReader relPath
      return $ tryParsePackageDescription =<< raw

createIndexReaderFor :: (Monad m, MonadIO m, MonadOutput m) => FilePath -> m (FilePath -> m (Maybe String))
createIndexReaderFor tarPath = do
  indexReaderRef <- liftIO $ newIORef noIndexFn
  liftIO $ writeIORef indexReaderRef (loadIndexFn indexReaderRef)
  return $ \relPath -> do
    -- "Self-modifying" code
    --   on first run it is 'loadIndexFn'
    --   after that it is either 'withLoadedIndexFn' or 'noIndexFn'
    fn <- liftIO $ readIORef indexReaderRef
    fn relPath

  where
    noIndexFn :: (Monad m) => FilePath -> m (Maybe String)
    noIndexFn =
      const (return Nothing)

    withLoadedIndexFn :: (Monad m) => Map.Map FilePath BL.ByteString -> FilePath -> m (Maybe String)
    withLoadedIndexFn mapping relPath =
      return $ (B.unpack . BL.toStrict) <$> Map.lookup relPath mapping

    loadIndexFn :: (Monad m, MonadIO m, MonadOutput m) => IORef (FilePath -> m (Maybe String)) -> FilePath -> m (Maybe String)
    loadIndexFn fnRef relPath = do
      tarContents <- liftIO $ BL.readFile tarPath
      let entries = Tar.read tarContents
          mapping = Tar.foldEntries addEntryToMap (Just Map.empty) (const Nothing) entries
      fn <- case mapping of
        Just mapping' -> do
          debug $ "Read " <> show (Map.size mapping') <> " entries"
          return $ withLoadedIndexFn mapping'
        Nothing ->
          return noIndexFn
      liftIO $ writeIORef fnRef fn
      fn relPath


    addEntryToMap :: Tar.Entry -> Maybe (Map.Map FilePath BL.ByteString) -> Maybe (Map.Map FilePath BL.ByteString)
    addEntryToMap _ Nothing = Nothing
    addEntryToMap e (Just m) =
      case Tar.entryContent e of
        Tar.NormalFile content _ -> Just (Map.insert (Tar.entryPath e) content m)
        _ -> Just m

tryParsePackageDescription :: String -> Maybe LightweightPackageDescription
tryParsePackageDescription src = case parsePackageDescription src of
  ParseOk _ d -> Just $ toLightweightPackageDescription d
  _           -> Nothing

toLightweightPackageDescription :: GenericPackageDescription -> LightweightPackageDescription
toLightweightPackageDescription pkgDesc =
  let finalizeResult = finalizePackageDescription
                         []             -- TODO: configurable flags
                         (const True)
                         buildPlatform  -- TODO: configurable platform
                         (unknownCompilerInfo
                          (CompilerId buildCompilerFlavor (Version [] [])) NoAbiTag)
                         []
                         pkgDesc
  in case finalizeResult of
       Left missingDeps -> MissingDependencies missingDeps
       Right (finalizedPkgDesc, _) ->
         LightweightPackageDescription
         { lpdName = pkgName (package finalizedPkgDesc)
         , lpdVersion = pkgVersion (package finalizedPkgDesc)
         , lpdDependencies = buildDepends finalizedPkgDesc
         }

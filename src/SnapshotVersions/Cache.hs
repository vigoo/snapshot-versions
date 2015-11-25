{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SnapshotVersions.Cache
       ( cached
       , cachedMaybe
       )
       where

import           Control.Monad.IO.Class
import qualified Data.ByteString         as B
import           Data.Maybe
import           Data.Serialize
import           Data.Version
import           Paths_snapshot_versions
import           System.Directory
import           System.FilePath

data Cached a =
  Cached { cVersion :: Version
         , cData    :: a
         }

instance (Serialize a) => Serialize (Cached a) where
  put Cached{..} = do
    put (versionBranch cVersion)
    put cData

  get = Cached <$> (makeVersion <$> get) <*> get

cached :: forall m a. (MonadIO m, Serialize a) => String -> m a -> m a
cached name fn = cached' name fn (const True)

cachedMaybe :: forall m a. (MonadIO m, Serialize a) => String -> m (Maybe a) -> m (Maybe a)
cachedMaybe name fn = cached' name fn isJust

cached' :: forall m a. (MonadIO m, Serialize a) => String -> m a -> (a -> Bool) -> m a
cached' name fn validator = do
  path <- liftIO $ cachePath name
  isCached <- liftIO $ doesFileExist path
  if isCached
  then do
    raw <- liftIO $ B.readFile path
    let res = decode raw
    case res of
      Right (Cached v d) | v == version && validator d -> return d
      _ -> runAndCache path
  else runAndCache path

  where
    runAndCache :: FilePath -> m a
    runAndCache path = do
      d <- fn
      let raw = encode (Cached version d)
      liftIO $ B.writeFile path raw
      return d

cachePath :: String -> IO FilePath
cachePath name = do
  home <- getHomeDirectory
  let cacheDir = home </> ".snapshot-versions" </> "cache"
  createDirectoryIfMissing True cacheDir
  return $ cacheDir </> name

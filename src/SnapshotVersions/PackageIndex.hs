module SnapshotVersions.PackageIndex where

import qualified Codec.Archive.Tar                     as Tar
import           Control.Applicative
import qualified Data.ByteString.Char8                 as B
import qualified Data.ByteString.Lazy                  as BL
import qualified Data.Map                              as Map
import           Data.Monoid
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import           SnapshotVersions.CmdLine
import           SnapshotVersions.Output
import           System.Directory
import           System.FilePath

type IndexReader = String -> String -> IO (Maybe GenericPackageDescription)

indexPath :: IO FilePath
indexPath = getHomeDirectory >>= \dir -> return (dir </> ".stack" </> "indices" </> "Hackage" </>"00-index.tar")

createIndexReader :: OutputType -> IO IndexReader
createIndexReader outputType = do
  path <- indexPath
  contentReader <- createIndexReaderFor outputType path
  return $ \name version -> do
    let relPath = name </> version </> (name <> ".cabal")
    return $ tryParsePackageDescription =<< (contentReader relPath)

createIndexReaderFor :: OutputType -> FilePath -> IO (FilePath -> Maybe String)
createIndexReaderFor outputType tarPath = do
  tarContents <- BL.readFile tarPath
  let entries = Tar.read tarContents
      mapping = Tar.foldEntries addEntryToMap (Just Map.empty) (const Nothing) entries
  case mapping of
    Just mapping' -> do
      debug outputType $ "Read " <> show (Map.size mapping') <> " entries"
      return $ \relPath -> (B.unpack . BL.toStrict) <$> Map.lookup relPath mapping'
    Nothing -> return $ const Nothing

  where
    addEntryToMap :: Tar.Entry -> Maybe (Map.Map FilePath BL.ByteString) -> Maybe (Map.Map FilePath BL.ByteString)
    addEntryToMap _ Nothing = Nothing
    addEntryToMap e (Just m) =
      case Tar.entryContent e of
        Tar.NormalFile content _ -> Just (Map.insert (Tar.entryPath e) content m)
        _ -> Just m

tryParsePackageDescription :: String -> Maybe GenericPackageDescription
tryParsePackageDescription src = case parsePackageDescription src of
  ParseOk _ d -> Just d
  _ -> Nothing
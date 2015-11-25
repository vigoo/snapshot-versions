{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
module SnapshotVersions.Snapshot where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Char
import qualified Data.Map                   as Map
import           Data.Monoid
import           Data.Serialize
import           Distribution.ParseUtils
import           Network.HTTP.Conduit
import           SnapshotVersions.Cache
import           SnapshotVersions.CmdLine
import           SnapshotVersions.Output
import           Text.PrettyPrint.HughesPJ  hiding ((<>))

newtype VersionMap = VersionMap { asMap :: Map.Map ByteString VersionInSnapshot }

newtype VersionMapReader m a =
  VersionMapReader { vmr :: ReaderT VersionMap m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader VersionMap, MonadTrans)

data VersionInSnapshot = ExplicitVersion ByteString
                       | InstalledGlobal

instance Serialize VersionInSnapshot where
  put InstalledGlobal = putWord8 0
  put (ExplicitVersion ver) = do
    putWord8 1
    put ver

  get = do
    tag <- getWord8
    case tag of
      0 -> return InstalledGlobal
      1 -> ExplicitVersion <$> get
      _ -> error "Invalid tag in cached version map"

instance Serialize VersionMap where
  put VersionMap{..} = put asMap
  get = VersionMap <$> get

class Monad m => MonadVersionMap m where
  getVersionMap :: m VersionMap

instance Monad m => MonadVersionMap (VersionMapReader m) where
  getVersionMap = ask

instance (MonadOutput m) => MonadOutput (VersionMapReader m) where
  indented fn = ask >>= \val -> lift $ indented (runReaderT (vmr fn) val)
  debug = lift . debug
  info = lift . info
  logError = lift . logError
  logWarning = lift . logWarning
  resultStart = lift resultStart
  resultEnd = lift resultEnd
  result p n v = lift $ result p n v

withVersionMap :: forall m. (Monad m, MonadIO m, MonadOutput m) => SnapshotName -> VersionMapReader m () -> m ()
withVersionMap name fn = do
  versionMap' <- cachedMaybe "versionmap" $ fetchVersionMap name
  case versionMap' of
    Nothing -> logError "Failed to fetch snapshot."
    Just versionMap -> runReaderT (vmr fn) versionMap


fetchVersionMap :: forall m. (Monad m, MonadIO m, MonadOutput m) => SnapshotName -> m (Maybe VersionMap)
fetchVersionMap name = do
  let url = "https://www.stackage.org/" <> name <> "/cabal.config"
  body <- liftIO $ simpleHttp url

  let res = extractRawConstraints body
  case res of
    ParseOk _ rawConstraints -> do
      let constraints = Map.fromList $ map (parseConstraint . strip) (B8.split ',' rawConstraints)
      return $ Just $ VersionMap constraints
    _ -> return Nothing

parseConstraint :: ByteString -> (ByteString, VersionInSnapshot)
parseConstraint entry =
  let [name, constr] = B8.words entry
  in if constr == "installed"
     then (name, InstalledGlobal)
     else (name, ExplicitVersion $ B.drop 2 constr)

extractRawConstraints :: BL.ByteString -> ParseResult ByteString
extractRawConstraints src =
    let desc = [ FieldDescr { fieldName = "constraints"
                            , fieldGet = const $ text ""
                            , fieldSet = \_ s _ -> pure s
                            }
               ]
    in B8.pack <$> parseFields desc "" (B8.unpack (BL.toStrict src))

strip :: ByteString -> ByteString
strip = B.reverse . lstrip . B.reverse . lstrip
  where
    lstrip = B8.dropWhile isSpace

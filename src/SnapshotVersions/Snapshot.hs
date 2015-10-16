{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
module SnapshotVersions.Snapshot where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.Monoid
import           Data.String.Utils
import           Distribution.ParseUtils
import           Network.HTTP.Conduit
import           SnapshotVersions.CmdLine
import           SnapshotVersions.Output
import           Text.PrettyPrint.HughesPJ  hiding ((<>))

newtype VersionMap = VersionMap { asMap :: Map.Map String VersionInSnapshot }

newtype VersionMapReader m a =
  VersionMapReader { vmr :: ReaderT VersionMap m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader VersionMap, MonadTrans)

data VersionInSnapshot = ExplicitVersion String | InstalledGlobal

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
  versionMap' <- fetchVersionMap name
  case versionMap' of
    Nothing -> logError "Failed to fetch snapshot."
    Just versionMap -> runReaderT (vmr fn) versionMap


fetchVersionMap :: forall m. (Monad m, MonadIO m, MonadOutput m) => SnapshotName -> m (Maybe VersionMap)
fetchVersionMap name = do
  let url = "https://www.stackage.org/" <> name <> "/cabal.config"
  body <- liftIO $ simpleHttp url

  let result = extractRawConstraints body
  case result of
    ParseOk _ rawConstraints -> do
      let constraints = Map.fromList $ map (parseConstraint . strip) (split "," rawConstraints)
      return $ Just $ VersionMap constraints
    _ -> return Nothing

parseConstraint :: String -> (String, VersionInSnapshot)
parseConstraint entry =
  let [name, constr] = words entry
  in if constr == "installed"
     then (name, InstalledGlobal)
     else (name, ExplicitVersion $ drop 2 constr)

extractRawConstraints :: BL.ByteString -> ParseResult String
extractRawConstraints src =
    let desc = [ FieldDescr { fieldName = "constraints"
                          , fieldGet = const $ text ""
                          , fieldSet = \_ s _ -> pure s
                          }
             ]
    in parseFields desc "" (B8.unpack $ BL.toStrict src)

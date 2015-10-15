module SnapshotVersions.Snapshot where

import           Control.Monad.IO.Class
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

data VersionInSnapshot = ExplicitVersion String | InstalledGlobal

fetchVersionMap :: SnapshotName -> OutputMonad IO (Maybe VersionMap)
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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
module SnapshotVersions.Output where

import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Monoid
import           SnapshotVersions.CmdLine
import           System.IO

data OutputState =
  OutputState { osIndent     :: String
              , osOutputType :: OutputType
              , osDebug      :: Bool
              }

newtype OutputMonad m a =
  OutputMonad { om :: StateT OutputState m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadState OutputState)

withOutput :: Monad m => OutputType -> Bool -> OutputMonad m () -> m ()
withOutput typ dbg fn = do
  _ <- runStateT (om fn) $ OutputState { osIndent = ""
                                      , osOutputType = typ
                                      , osDebug = dbg
                                      }
  return ()

indented :: (MonadIO m) => OutputMonad m a -> OutputMonad m a
indented fn = do
  saved <- get
  modify (\os -> os { osIndent = (osIndent os) <> "  " })
  r <- fn
  put saved
  return r

debug :: (MonadIO m) => String -> OutputMonad m ()
debug s = get >>= \OutputState{..} ->
  if osDebug
  then liftIO $ putStrLn $ osIndent <> s
  else return ()

info :: (MonadIO m) => String -> OutputMonad m ()
info s = get >>= \OutputState{..} ->
  case osOutputType of
    Default -> liftIO $ putStrLn $ osIndent <> s
    _ -> liftIO $ hPutStrLn stderr $ osIndent <> s

logError :: (MonadIO m) => String -> OutputMonad m ()
logError s = get >>= \OutputState{..} ->
  liftIO $ hPutStrLn stderr $ osIndent <> s

logWarning :: (MonadIO m) => String -> OutputMonad m ()
logWarning s = get >>= \OutputState{..} ->
  liftIO $ hPutStrLn stderr $ osIndent <> "Warning: " <> s

resultStart :: (MonadIO m) => OutputMonad m ()
resultStart = get >>= \OutputState{..} ->
  case osOutputType of
    Default -> liftIO $ putStrLn "Results:"
    CabalConstraints -> liftIO $ putStr "constraints: "
    _ -> return ()

resultEnd :: (MonadIO m) => OutputMonad m ()
resultEnd = return ()

result :: (MonadIO m) => String -> String -> OutputMonad m ()
result name ver = get >>= \OutputState{..} ->
  case osOutputType of
    Default -> liftIO $ putStrLn $ name <> ": " <> ver
    StackYaml -> liftIO $ putStrLn $ name <> "-" <> ver
    CabalConstraints -> liftIO $ putStrLn $ "             " <> name <> " ==" <> ver <> ","

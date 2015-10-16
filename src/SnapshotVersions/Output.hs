{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
module SnapshotVersions.Output where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
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
  deriving (Applicative, Functor, Monad, MonadIO, MonadState OutputState, MonadTrans)

data ListPos = ListStart
             | ListMiddle
             | ListEnd
             deriving (Show, Eq)

toListPos :: Int -> Int -> ListPos
toListPos 0 _ = ListStart
toListPos i l | i < (l-1) = ListMiddle
toListPos _ _ = ListEnd

withOutput :: Monad m => OutputType -> Bool -> OutputMonad m () -> m ()
withOutput typ dbg fn = do
  _ <- runStateT (om fn) $ OutputState { osIndent = ""
                                      , osOutputType = typ
                                      , osDebug = dbg
                                      }
  return ()

class Monad m => MonadOutput m where
  indented :: m a -> m a
  debug :: String -> m ()
  info :: String -> m ()
  logError :: String -> m ()
  logWarning :: String -> m ()
  resultStart :: m ()
  resultEnd :: m ()
  result :: ListPos -> String -> String -> m ()

instance (Monad m, MonadIO m) => MonadOutput (OutputMonad m) where
  indented fn = do
    saved <- get
    modify (\os -> os { osIndent = (osIndent os) <> "  " })
    r <- fn
    put saved
    return r

  debug s = get >>= \OutputState{..} ->
    if osDebug
    then liftIO $ putStrLn $ osIndent <> s
    else return ()

  info s = get >>= \OutputState{..} ->
    case osOutputType of
      Default -> liftIO $ putStrLn $ osIndent <> s
      _ -> liftIO $ hPutStrLn stderr $ osIndent <> s

  logError s = get >>= \OutputState{..} ->
    liftIO $ hPutStrLn stderr $ osIndent <> s

  logWarning s = get >>= \OutputState{..} ->
    liftIO $ hPutStrLn stderr $ osIndent <> "Warning: " <> s

  resultStart = get >>= \OutputState{..} ->
    case osOutputType of
      Default -> liftIO $ putStrLn "Results:"
      CabalConstraints -> liftIO $ putStr "constraints: "
      _ -> return ()

  resultEnd = return ()

  result pos name ver = get >>= \OutputState{..} ->
    case osOutputType of
      Default -> liftIO $ putStrLn $ name <> ": " <> ver
      StackYaml -> liftIO $ putStrLn $ name <> "-" <> ver
      CabalConstraints ->
        case pos of
          ListStart  -> liftIO $ putStrLn $ name <> " ==" <> ver <> ","
          ListMiddle -> liftIO $ putStrLn $ "             " <> name <> " ==" <> ver <> ","
          ListEnd    -> liftIO $ putStrLn $ "             " <> name <> " ==" <> ver

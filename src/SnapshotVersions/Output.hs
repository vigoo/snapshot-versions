module SnapshotVersions.Output where

import           Data.Monoid
import           SnapshotVersions.CmdLine
import           System.IO

debug :: OutputType -> String -> IO ()
debug Default s = putStrLn s
debug _ _ = return ()

logError :: OutputType -> String -> IO ()
logError _ s = hPutStrLn stderr s

logWarning :: OutputType -> String -> IO ()
logWarning _ s = hPutStrLn stderr $ "Warning: " <> s

resultStart :: OutputType -> IO ()
resultStart Default = putStrLn "Results:"
resultStart CabalConstraints = putStr "constraints: "
resultStart _ = return ()

resultEnd :: OutputType -> IO ()
resultEnd _ = return ()

result :: OutputType -> String -> String -> IO ()
result Default name ver = putStrLn $ name <> ": " <> ver
result StackYaml name ver = putStrLn $ name <> "-" <> ver
result CabalConstraints name ver = putStrLn $ "             " <> name <> " ==" <> ver <> ","

{-# LANGUAGE TemplateHaskell, BangPatterns #-}

module ManyCores where

import System.Environment
import Data.List
import System.IO
import System.Argv0
import System.Process
import System.Exit (ExitCode, die)
import Data.Maybe (fromJust)
import Data.List.Split (splitOn)
import Filesystem.Path.CurrentOS
import Control.Concurrent
import Control.Monad
import Control.Distributed.Process
import Control.Distributed.Process.Internal.Primitives (finally)
import Control.Distributed.Process.Serializable
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import qualified Control.Distributed.Process.Node as Node
import qualified Control.Distributed.Process as Ps

import Control.Concurrent.MVar

{-
slave :: ProcessId -> (a -> b) -> Process ()
slave them f = forever $ do
    n <- expect
    send them (f n)

remotable ['slave, 'f]

rtable :: RemoteTable
rtable = __remoteTable initRemoteTable
-}


getExecuteInfo :: IO (String, [String])
getExecuteInfo = do
    args <- getArgs
    path <- getArgv0
    let file_name = encodeString path
    return (file_name, args)

mcoreStartMaster :: Serializable a => Backend -> Int -> ([NodeId] -> MVar a -> Process()) -> IO (a)
mcoreStartMaster backend n proc = do
    node <- newLocalNode backend
    result <- newEmptyMVar
    Node.runProcess node $ do
        slaves <- waitSlave n backend
        redirectLogsHere backend slaves
        proc (map processNodeId slaves) result `finally` shutdownLogger
    ret <- takeMVar result
    return ret
    where
        waitSlave :: Int -> Backend -> Process [ProcessId]
        waitSlave n backend = do
            tmp_slaves <- findSlaves backend
            if ((length tmp_slaves) < n) then
                waitSlave n backend
            else
                return tmp_slaves
        shutdownLogger :: Process ()
        shutdownLogger = do
            (sport, rport) <- Ps.newChan
            nsend "logger" (sport :: SendPort ())
            receiveChan rport


getProcessOutput :: String -> IO (String, ExitCode)
getProcessOutput command = do
    (_pin, pOut, pErr, handle) <- runInteractiveCommand command
    exitCode <- waitForProcess handle
    output <- hGetContents pOut
    return (output, exitCode)

isInteger :: String -> Bool
isInteger s = case reads s :: [(Integer, String)] of
    [(_, "")] -> True
    _         -> False

pickPort :: Int -> IO (Maybe String)
pickPort n = do
    (used, _) <- getProcessOutput "lsof -i -nP | grep LISTEN | awk '{print $(NF-1)}' | sort -u | awk -F ':' '{print $NF}'"
    let ports = filter (\s -> isInteger s) $ splitOn "\n" used
    let result = searchPort n ports
    case result of
        Nothing -> return Nothing
        Just port -> return (Just port)
    where
        searchPort :: Int -> [String] -> Maybe String
        searchPort 1024 _ = Nothing
        searchPort candidate usedPorts = 
            if elem (show candidate) usedPorts then
                searchPort (candidate + 1) usedPorts
            else
                Just (show candidate)

checkSlaveMode :: [String] -> Bool
checkSlaveMode [] = False
checkSlaveMode ns = if ((last ns) == "slave") then 
                        True
                    else
                        False

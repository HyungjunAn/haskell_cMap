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

import Language.Haskell.TH.Syntax
import Language.Haskell.TH


{-sslave :: Serializable a => (ProcessId, a -> a) -> Process ()-}
{-sslave :: (ProcessId, Int -> Int) -> Process ()
sslave (them, f) = forever $ do
    n <- expect
    let ret = f n
    send them ret-}

mkSlave fName = [| \them -> forever $ do
                          n <- expect
                          let ret = $(varE fName) n
                          send them ret |]

runSlave nThread  n = [| 
    let threads = [1..nThread]
    in do
      forM_ threads $ \thread -> do
        port <- pickPort (n + thread)
        backend <- initializeBackend "127.0.0.1" (fromJust port) ($(varE (mkName "__remoteTable")) initRemoteTable)
        startSlave backend |]

--runMaster  :: Serializable a => Int -> Int -> Name -> [a] -> IO [Int]
runMaster name = [| \n -> \nThread -> \src -> do
    port <- pickPort n
    backend <- initializeBackend "127.0.0.1" (fromJust port) ($(varE (mkName "__remoteTable")) initRemoteTable)
    ret <- mcoreStartMaster backend nThread $ \slaves mResult -> do
        us <- getSelfPid
        slaveProcesses <- forM slaves $ \nid -> spawn nid ($(mkClosure name) us)
        spawnLocal $ forM_ (zip src (cycle slaveProcesses)) $ \ (m, them) -> send them m
        let src_size = length src
        ret <- mergeList src_size
        --liftIO $ print ret
        liftIO $ putMVar mResult ret
    return ret |]
{-    where
        mergeList :: Serializable a => Int -> Process [a]
        mergeList = go []
            where
                go :: Serializable a => [a] -> Int -> Process [a]
                go !acc 0 = return acc
                go !acc n = do
                    m <- expect 
                    go (acc ++ [m]) (n-1)
-}
mergeList :: Int -> Process [Int]
mergeList = go []
    where
    go :: [Int] -> Int -> Process [Int]
    go !acc 0 = return acc
    go !acc n = do
        m <- expect 
        go (acc ++ [m]) (n-1)

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

getHostIp :: IO (Maybe String)
getHostIp = do
    ip <- getProcessOutput $
                    "ifconfig | grep inet | head -n 1  | awk -F ':' '{print $2}' | awk '{print $1}'"
    case ip of 
        "" -> return   Nothing
        _  -> return $ Just (take (length ip - 1) ip) -- remove newline
    where
        getProcessOutput :: String -> IO String
        getProcessOutput command = do
              (_pin, pOut, pErr, handle) <- runInteractiveCommand command
              output <- hGetContents pOut
              return output

checkSlaveMode :: [String] -> Bool
checkSlaveMode [] = False
checkSlaveMode ns = if ((last ns) == "slave") then 
                        True
                    else
                        False

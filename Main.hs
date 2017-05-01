{-# LANGUAGE TemplateHaskell, BangPatterns #-}

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
import Control.Distributed.Process.Serializable  
import Control.Concurrent.MVar
import ManyCores

fibo :: Int -> Int
fibo 0 = 1
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

slave :: (Int -> Int) -> ProcessId -> Process ()
slave f them = forever $ do
    n <- expect
    send them (f n)

test = slave fibo
remotable ['test]

rtable :: RemoteTable
rtable = __remoteTable initRemoteTable
{-
slave :: ProcessId -> Process ()
slave them = forever $ do
    n <- expect
    let ret = fibo n
    send them ret


remotable ['slave]

rtable :: RemoteTable
rtable = __remoteTable initRemoteTable
-}

runSlave :: Int -> Int -> IO ()
runSlave nThread  n = do
    forM_ threads $ \thread -> do
        port <- pickPort (n + thread)
        backend <- initializeBackend "127.0.0.1" (fromJust port) rtable
        startSlave backend
    where
        threads = [1..nThread]

mergeList :: Int -> Process [Int]
mergeList = go []
    where
    go :: [Int] -> Int -> Process [Int]
    go !acc 0 = return acc
    go !acc n = do
        m <- expect 
        go (acc ++ [m]) (n-1)


runMaster  :: Serializable a => Int -> Int -> [a] -> IO [Int]
runMaster n nThread src = do
    port <- pickPort n
    backend <- initializeBackend "127.0.0.1" (fromJust port) rtable
    ret <- mcoreStartMaster backend nThread $ \slaves mResult -> do
        us <- getSelfPid
        slaveProcesses <- forM slaves $ \nid -> spawn nid ($(mkClosure 'test) us)
        spawnLocal $ forM_ (zip src (cycle slaveProcesses)) $ \ (m, them) -> send them m
        let src_size = length src
        ret <- mergeList src_size
        --liftIO $ print ret
        liftIO $ putMVar mResult ret
    return ret
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
                       
main = do
    (exe, args) <- getExecuteInfo
    nThread <- getNumCapabilities 
    if (checkSlaveMode args) then
        do
            let n = read $ last $ take ((length args) -1) args
            port <- pickPort (100+n)
            backend <- initializeBackend "127.0.0.1" (fromJust port) rtable
            --print $ "slave.... " ++ (fromJust port)
            startSlave backend
    else
        do
            print "Master Mode"
            
            handles <- forM [1..nThread] $ \i -> do
                let arg_string = intercalate " " args
                handle <- spawnProcess exe (args ++ [(show i)] ++ ["slave"]) 
                return handle
            print "Run...."
            let inp = ([1..40] :: [Int])
            ret <- runMaster 80 nThread inp
            print "End...."
            print ret
            mapM_ terminateProcess handles

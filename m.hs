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
import qualified Control.Distributed.Process.Node as Node
import qualified Control.Distributed.Process as Ps


getExecuteInfo :: IO (String, [String])
getExecuteInfo = do
    args <- getArgs
    path <- getArgv0
    let file_name = encodeString path
    return (file_name, args)

fibo :: Int -> Int
fibo 0 = 1
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

slave :: ProcessId -> Process ()
slave them = forever $ do
    n <- expect
    let ret = fibo n
    --liftIO $ print ret
    send them ret


remotable ['slave]

rtable :: RemoteTable
rtable = __remoteTable initRemoteTable


runSlave :: Int -> Int -> IO ()
runSlave nThread  n = do
    forM_ threads $ \thread -> do
        port <- pickPort (n + thread)
        backend <- initializeBackend "127.0.0.1" (fromJust port) rtable
        startSlave backend
    where
        threads = [1..nThread]

mcoreStartMaster :: Backend -> Int -> ([NodeId] -> Process()) -> IO ()
mcoreStartMaster backend n proc = do
    node <- newLocalNode backend
    Node.runProcess node $ do
        slaves <- findSlaves backend
        redirectLogsHere backend slaves
        proc (map processNodeId slaves) `finally` shutdownLogger
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

runMaster  :: Serializable a => Int -> [a] -> IO ()
runMaster n src = do
    port <- pickPort n
    backend <- initializeBackend "127.0.0.1" (fromJust port) rtable
    startMaster backend $ \slaves -> do
        tmp <- findSlaves backend
        let sv = map processNodeId tmp
        --liftIO $ print $ show sv
        us <- getSelfPid
        --liftIO $ print $ show slaves
        slaveProcesses <- forM sv $ \nid -> spawn nid ($(mkClosure 'slave) us)
        spawnLocal $ forM_ (zip src (cycle slaveProcesses)) $ \ (m, them) -> send them m
        let src_size = length src
        --liftIO $ print "End Send"
        ret <- mergeList src_size
        liftIO $ print ret
        --liftIO $ print "End"
        return ()
    where
        mergeList :: Int -> Process [Int]
        mergeList = go []
            where
                go :: [Int] -> Int -> Process [Int]
                go !acc 0 = return acc
                go !acc n = do
                    --liftIO $ print "Wait"
                    m <- expect 
                    --liftIO $ print "End"
                    go (acc ++ [m]) (n-1)

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
                --print (args ++ [(show i)] ++ ["slave"])
                handle <- spawnProcess exe (args ++ [(show i)] ++ ["slave"]) 
                return handle
            print "Run...."
            --(result, _) <- getProcessOutput "ps"
            --print $ result
            --print $ length handles
            let inp = ([1..40] :: [Int])
            runMaster 80 inp
            print "End...."
            mapM_ terminateProcess handles
            --print nThread
            --print args
            --print $ exe

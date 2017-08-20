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

import Language.Haskell.TH.Syntax
import Language.Haskell.TH

fibo :: Int -> Int
fibo 0 = 1
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)


slave :: ProcessId -> Process ()
slave = $(mkSlave 'fibo)

--remotable ['slave, 'sslave]
remotable ['slave]

main = do
    (exe, args) <- getExecuteInfo
    nThread <- getNumCapabilities 
    case args of 
      ["slave"] -> do
            $runSlave 80 nThread
      ["master"] -> do
            {-print "Master Mode"
            handles <- forM [1..nThread] $ \i -> do
                let arg_string = intercalate " " args
                handle <- spawnProcess exe (args ++ [(show i)] ++ ["slave"]) 
                return handle
            print "Run...."-}
            let inp = ([1..4] :: [Int])
            ret <- $(runMaster 'slave) 80 nThread inp
            {-print "End...."-}
            print ret
            {-mapM_ terminateProcess handles-}

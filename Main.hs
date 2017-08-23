{-# LANGUAGE TemplateHaskell #-}

import System.Environment
import System.Argv0
import System.Exit (ExitCode, die)
import Filesystem.Path.CurrentOS
import Control.Concurrent
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import ManyCores

input = [1 .. 100] :: [Int]

incr :: Int -> Int
incr x = x + 1

afterFunc :: [Int] -> IO ()
afterFunc xs = putStrLn $ show $ sum xs
--------------------------------------------------------------------------
slaveJob :: ProcessId -> Process ()
slaveJob = $(mkSlave 'incr)

remotable ['slaveJob]

main = do
    (exe, args) <- getExecuteInfo
    nThread <- getNumCapabilities 
    case args of 
      ["slave"] -> do
            $runSlave 80 nThread
      ["master"] -> do
            ret <- $(runMaster 'slaveJob) 80 nThread input
            afterFunc ret

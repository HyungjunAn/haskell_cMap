import Control.Parallel.Strategies
import System.Environment

fibo :: Int -> Int
fibo 0 = 1
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)


main :: IO () 
main = do 
    args <- getArgs
    let list = [1..40]

    case args of
        ["seq"] -> do
            let results = map fibo list
            print results
        ["par"] -> do
            let results = parMap rseq fibo list
            print results
